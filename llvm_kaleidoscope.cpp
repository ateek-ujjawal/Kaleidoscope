#include "include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

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

    llvm::Function *codegen();
    const std::string &getName() const { return Name; }
};

// Class for representing the definition of a function
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

  public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {}

    llvm::Function *codegen();
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

    auto Proto = std::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

//////////////////////////////////// Code Generation /////////////////////////////
static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::Value*> NamedValues;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

Function *getFunction(std::string Name) {
  // First, see if function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null
  return nullptr;
}

// Code generation for numeric literals
// Represented as a constant floating point value
llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

// Code generation for variable references
// Looks up variables in the NamedValues map
llvm::Value *VariableExprAST::codegen() {
  llvm::Value *V = NamedValues[Name];
  if (!V)
    LogErrorV("unknown variable name");
  return V;
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();

  if (!L || !R)
    return nullptr;

  switch(Op) {
    case '+':
      return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
      return Builder->CreateFSub(L, R, "subtmp");
    case '*':
      return Builder->CreateFMul(L, R, "multmp");
    case '<':
      L = Builder->CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext), "booltmp");
    default:
      return LogErrorV("invalid binary operator");
  }
}

llvm::Value *CallExprAST::codegen() {
  // Lookup name in global module table;
  llvm::Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // Argument mismatch error
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<llvm::Value*> ArgsV;
  for (unsigned int i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function* PrototypeAST::codegen() {
  // Make function type: double(double, double...) etc.
  std::vector<llvm::Type*> Doubles(Args.size(), llvm::Type::getDoubleTy(*TheContext));

  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext), Doubles, false);

  llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

llvm::Function* FunctionAST::codegen() {
  // Transfer ownership of prototype to FunctionProtos map, keep
  // reference to it for use below
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());

  if (!TheFunction)
    return nullptr;

  // Create a basic block to start insertion into
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record function arguments in NamedValues map
  NamedValues.clear();

  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;

  if (llvm::Value *RetVal = Body->codegen()) {
    // Finish off the function
    Builder->CreateRet(RetVal);

    // Validate generated code, checking for consistency
    verifyFunction(*TheFunction);

    // Optimize the function
    TheFPM->run(*TheFunction, *TheFAM);

    return TheFunction;
  }

  // Error reading the body, delete function
  TheFunction->eraseFromParent();
  return nullptr;
}

////////////////////////////// Top-level Parsing //////////////////////
static void InitializeModuleAndManagers() {
  // Open a new context and module.
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("KaleidoscopeJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  // Create new pass and analysis managers
  TheFPM = std::make_unique<FunctionPassManager>();
  TheLAM = std::make_unique<LoopAnalysisManager>();
  TheFAM = std::make_unique<FunctionAnalysisManager>();
  TheCGAM = std::make_unique<CGSCCAnalysisManager>();
  TheMAM = std::make_unique<ModuleAnalysisManager>();
  ThePIC = std::make_unique<PassInstrumentationCallbacks>();
  TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                    /*DebugLogging*/ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());

  // Add transform passes
  // Do simple 'peephole' optimizations and bit-twiddling optzns
  TheFPM->addPass(InstCombinePass());
  // Reassociate expressions
  TheFPM->addPass(ReassociatePass());
  // Eliminate Common SubExpressions
  TheFPM->addPass(GVNPass());
  // Simplify Control Flow Graph (delete unreachable blocks, etc.)
  TheFPM->addPass(SimplifyCFGPass());

  // Register analysis passes used in these transform passes
  PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(
          ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndManagers();
    }
  } else {
    // skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()){
      // Create resource tracker to track JIT'd memory allocated
      // to our anonymous expression, to free it after execution
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModuleAndManagers();

      // Search the JIT for the __anon_expr symbol
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get symbol's address and cast it to the right type so
      // we can call it as a native function
      double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete anonymous expression module from the JIT
      ExitOnErr(RT->remove());
    }
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

////////////////////////////////// Driver code /////////////////////////
int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  // Install standard binary operators
  // 1 is the lowest precedence according to this order
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

  // Make the module, which holds all the code.
  InitializeModuleAndManagers();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  TheModule->print(llvm::errs(), nullptr);

  return 0;
}