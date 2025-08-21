Kaleidoscope
=

A compiler written in C++ for the Kaleidoscope language based on the LLVM tutorials. Includes a hand written parser, lexer, AST nodes, grammar etc.

Features
-
- Hand written recursive descent parser and lexer for precision and granularity.
- Classes for different AST nodes to be generated according to the grammar(like Variables, Functions, Function Calls, Binary expressions etc.)
- Code generation using LLVM APIs in LLVM IR form.
- JIT and optimizer support with many transform and analysis passes.
- Supports only variables of type 'double'.
- Generates PHI nodes for if/then/else blocks.
- Ability to define custom operators with a custom precedence.
- Supports variable mutation using SSA(Static Single Assignment) form.
