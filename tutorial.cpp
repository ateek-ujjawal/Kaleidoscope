#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <map>
#include <memory>
#include <string>
#include <vector>

////////////////////////////////////////// Lexer /////////////////////////////////////////////

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.

enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

// get next token from standard input
static int gettok() {
  static int LastChar = ' ';

  // skips whitespaces
  while(isspace(LastChar))
    LastChar = getchar();

  // Matches to regex: [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar()))) 
      IdentifierStr += LastChar;
    
    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern;
    return tok_identifier;
  }

  // Matches to regex: [0-9.]+
  if (isdigit(LastChar) || LastChar == '.') {
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
  }

  // Handle # based comments
  if (LastChar == '#') {
    do
    {
      LastChar = getchar();
    } while (LastChar != EOF || LastChar != '\n' || LastChar != '\r');
    
    if (LastChar != EOF)
      return gettok();
  }

  // Return EOF
  if (LastChar == EOF)
    return tok_eof;
  
  // Return the character as an integer representation(it's ASCII value)
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

////////////////////////////////////// AST definition ////////////////////////////////////////

// Base class for expression nodes in AST
class ExprAST {
  public:
    virtual ~ExprAST() = default;
    virtual llvm::Value *codegen() = 0;
};

// Class for expressing numeric values(1.0) in AST
class NumberExprAST : public ExprAST {
  double Val;

  public:
    NumberExprAST(double Val) : Val(Val) {}
    llvm::Value *codegen() override;
};

// Class for expressing variables(var) in AST
class VariableExprAST : public ExprAST {
  std::string Name;

  public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    llvm::Value *codegen() override;
};

// Class for expressing binary operators(+, -) in AST
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

  public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, 
                  std::unique_ptr<ExprAST> RHS)
    : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
};

// Class for expressing function calls and their arguments in AST
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

  public:
    CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args)
    : Callee(Callee), Args(std::move(Args)) {}
    llvm::Value *codegen() override;
};

// Class for expressing the "prototype" of a function, i.e
// the function name and it's argument names
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

  public:
    PrototypeAST(const std::string &Name, std::vector<std::string> Args)
    : Name(Name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
};

// Class for representing the definition of a function
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

  public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

///////////////////////////////// Parser //////////////////////////////////
// Store the result of gettok() which gives a token in CurTok
// CurTok is the current token that the parser is looking at
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

// Error logging functions for Expressions and function prototypes
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// Parse a number value into a number expr AST node
// numberexpr := number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(Result);
}

// Parse parentheses around expressions
// parenexpr := '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // Eat (. to parse the expression inside
  auto V = ParseExpression();
  if (!V)
    return nullptr;
  
  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // Eat ).
  return V;
}

// Parse identifiers(variable references) and function calls
// identifierexpr := identifier | identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat the identifier

  if (CurTok != '(') // Variable reference
    return std::make_unique<VariableExprAST>(IdName);
  
  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while(true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;
      
      if (CurTok == ')')
        break;
      
      if (CurTok != ',')
        return LogError("Expected '(' or ',' in argument list");
      getNextToken();
    }
  }

  // eat ).
  getNextToken();
  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

//Parse primary expressions
// primary := numberexpr | identifierexpr | parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch(CurTok) {
    default:
      return LogError("unknown token when expecting an expression");
    case tok_identifier:
      return ParseIdentifierExpr();
    case tok_number:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
  }
}

// Map that holds precedence for each binary operator that is defined
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get precedence of the binary operator token
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

// binoprhs := ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binary operator, find it's precedence
  while (true) {
  int TokPrec = GetTokPrecedence();

  // If this is a binary operator that binds as tightly as
  if (TokPrec < ExprPrec)
  return LHS;

  int BinOp = CurTok;
  getNextToken();

  // Parse primary expressions after binary operator
  auto RHS = ParsePrimary();
  if (!RHS)
  return nullptr;

  // If binop binds less tightly with RHS, than with the
  // binary operator after RHS, let the pending operator take RHS as it's LHS
  int NextPrec = GetTokPrecedence();
  if (TokPrec < NextPrec) {
  RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS));
  if (!RHS)
  return nullptr;
  }

  // Merge LHS/RHS
  LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

// expression := primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

// prototype := id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  
  // Read list of argument names
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");
  
  // success
  getNextToken(); // eat ')'

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

// definition := 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat def.

  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;
  
  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

// external := 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat 'extern'.

  return ParsePrototype();
}

// toplevelexpr := expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make anonymous proto

    auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

////////////////////////////// Top-level Parsing //////////////////////
static void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern.\n");
  } else {
    // skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expression.\n");
  } else {
    // skip token for error recovery.
    getNextToken();
  }
}

// top := defintion | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
      case tok_eof:
        return;
      case ';': // ignore top-level semicolons
        getNextToken();
        break;
      case tok_def:
        HandleDefinition();
        break;
      case tok_extern:
        HandleExtern();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}

//////////////////////////////////// Code Generation /////////////////////////////
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::map<std::String, llvm::Value*> NamedValues;

Value *LogErrorV(const char *Str) {
  LogError(str);
  return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

llvm::Value *VariableExprAST::codegen() {
  Value *V = NamedValues[Name];
  if (!V)
    LogErrorV("unknown variable name");
  return V;
}

////////////////////////////////// Driver code /////////////////////////
int main() {
  // Install standard binary operators
  // 1 is the lowest precedence according to this order
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}