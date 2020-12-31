# Limited Functionality C Recursive Descent Parser
This project is an implementation of a predictive recursive descent parser for a version of C with limited functionality. It is broken up into four main project files: lexer.ml, parser.ml, eval.ml, and compiler.ml. Most of the source code was written as a part of a project for Professor Anwar Mamat's CMSC330 Organization of Programming Languages course at the University of Maryland, College Park.

I will first describe each main .ml file and what they do for the project as a whole, but there is a section at the bottom just demonstrating functionality.
## lexer.ml 
 #### `tokenize`

- **Type:** `string -> token list` 
- **Description:** Converts the C code into a list of tokens. The tokens themselves are represented as a type defined in tokenTypes.ml, which holds representations for all possible tokens this version of C can handle. The list of tokens outputted will always end with the 'EOF' token defined in tokenTypes.ml.
- **Examples:**
  ```ocaml
  tokenize "1 + 2" = [Tok_Int 1; Tok_Add; Tok_Int 2; EOF]
  tokenize "" = [EOF]
  tokenize "int main() { int x; }" = [Tok_Int_Type; Tok_Main; Tok_LParen; Tok_RParen; Tok_LBrace; Tok_Int_Type; Tok_ID "x"; Tok_Semi; Tok_RBrace; EOF]
  ```
  
## parser.ml

Once the tokens are created, parser.ml has three functions that process the list of tokens into an Abstract Syntax Tree to be evaluated by eval.ml. The C program is made up of statements such as: variable declarations, variable assignments, printf's, if statements, etc. as well as expressions such as integer and boolean operations. parser.ml has two functions that parse the expressions and statements making up the entire C program, and one function that particularly matches the main declaration.

The result of this is that parse_main (which uses parse_stmt and parse_expr) can be called on a valid C program within the CFG's functionality and it will return an Abstract Syntax Tree for the whole program defined by the types in smallCTypes.ml

#### `parse_expr`

- **Type:** `token list -> token list * expr`
- **Description:** Takes a list of tokens and returns an AST representing the expression corresponding to the given tokens, along with the new, reduced list of tokens represented as a tuple.
- **Exceptions:** If the next tokens in the token list do not represent an expression, an `InvalidInputException` is raised.
- **Examples:**
  ```ocaml
  parse_expr [Tok_Int(1); Tok_Add; Tok_Int(2); Tok_Semi; EOF] = ([Tok_Semi; EOF], Add (Int 1, Int 2))
  parse_expr [Tok_Int(1); EOF] = ([EOF], Int 1)
  parse_expr [Tok_Int_Type; EOF] (* InvalidInputException *)
  ```

#### `parse_stmt`

- **Type:** `token list -> token list * stmt`
- **Description:** Takes a list of tokens and returns an AST representing the statement corresponding to the given tokens, along with the new, reduced list of tokens represented as a tuple.
- **Exceptions:** If the next tokens in the token list do not represent a statement, an `InvalidInputException` is raised.
- **Examples:**
  ```ocaml
  parse_stmt [Tok_Int_Type; Tok_ID("x"); Tok_Semi; EOF] = ([EOF], Seq (Declare (Int_Type, "x"), NoOp))
  parse_stmt [Tok_ID("x"); Tok_Assign; Tok_Int(3); Tok_Semi; EOF] = ([EOF], Seq (Assign ("x", Int 3), NoOp))
  parse_stmt [Tok_Int(3); Tok_Add; Tok_Int(4); EOF] (* InvalidInputException *)
  ```

#### `parse_main`

- **Type:** `token list -> stmt`
- **Description:** Takes a list of tokens and returns an AST representing the list of statements in the body of the `main` function. This time all of the tokens will be consumed, because this is parsing the entirety of the main statement, and therefore the rest of the environment does not need to be returned.
- **Exceptions:** If the list of tokens does not contain only `EOF` at the end, an `InvalidInputException` is raised.  Also, if the token list contains tokens before the beginning of the `main` function declaration, or after it's last closing brace, or if the `main` function doesn't exist in the token list, an `InvalidInputException` is raised.
- **Examples:**
  ```ocaml
  parse_main [Tok_Int_Type; Tok_Main; Tok_LParen; Tok_RParen; Tok_LBrace; Tok_Int_Type; Tok_ID("x"); Tok_Semi; Tok_RBrace; EOF] = Seq (Declare (Int_Type, "x"), NoOp)
  parse_main [Tok_Int(3); Tok_Add; Tok_Int(4); EOF] (* InvalidInputException *)
  ```
## eval.ml

The formal operational semantics of this language can be found in `semantics.pdf`. The following error cases are not handled in the formal semantics but are handled in this RDP implementation:
- Any expression containing division by zero raises a `DivByZero` error when evaluated.
- Any expression or statement that is applied to the wrong types raises a `TypeError` exception when evaluated.
- An expression or statement that redefines an already defined variable, assigns to an undefined variable, or reads from an undefined variable raises a `DeclareError` when evaluated.

#### `eval_expr`

- **Type:** `environment -> expr -> value` 
- **Description:** Takes an environment `env` and an expression `e` and produces the result of evaluating `e`, which is of type `value` (`Int_Val` or `Bool_Val`) defined in smallCTypes.ml.
- **Examples:**
  ```ocaml
  eval_expr Add(Int(5), Int(6)) = Int_Val(11)
  eval_expr Add(Int(5), Mult(Int(2), Int(3)) = Int_Val(11)
  eval_expr And(Bool(true), Bool(false)) = Bool_Val(false)
  ```
#### `eval_stmt`

- **Type:** `environment -> stmt -> environment` 
- **Description:** Takes an environment `env` and a statement `s` and produces the result of evaluating `s`, which is of type `environment` (a (string * value) list) defined in smallCTypes.ml. This environment is represented as `a` in the formal semantics found in `semantics.pdf`. `env` can be passed as an existing environment or an empty list can be passed for an empty environment.
- **Examples:**
  ```ocaml
  eval_stmt [] Seq(Declare(Int_Type, "x"), Assign("x", Int(5))) = [("x", 5)]
  eval_stmt [] Seq(Declare(Int_Type, "x"), Seq(Declare(Bool_Type, "y"), Seq(Assign("x", Int(5)), Assign("x", Bool(true))))) = [("x", Int_Val(5)); ("y", Bool_Val(true))]
  ```
## compiler.ml
This is essentially the "main" method of the program. When invoked with a command line argument representing a file path to a text document containing C code, it will compile and run the C code using the previous three .ml files, and print out the resulting environment.

# Functionality
To run the compiler, the .ml files can be compiled together using (assuming the current directory is src):
```
ocamlopt -o compile str.cmxa tokenTypes.ml smallCTypes.ml lexer.mli lexer.ml utils.ml parser.mli parser.ml evalUtils.ml eval.mli eval.ml compiler.mli compiler.ml
```
Which will compile them into an executable called `compile`. That executable can simply be run with a file path to a C program text file, which will then be compiled and run. I have included `example.txt` into this repository to show just a few operations that can be performed using this parser. So, running:
```
compile example.txt
```
will have the following output:
```
10
false
true
4
8
12
*** BEGIN POST-EXECUTION ENVIRONMENT REPORT ***
- a => false
- b => true
- i => 15
- j => 3
- k => 4
- x => 15
- y => 12
***  END POST-EXECUTION ENVIRONMENT REPORT  ***
```
