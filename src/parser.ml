open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  let (toks_after_parse_orexpr, expr) = parse_orexpr toks in
    (toks_after_parse_orexpr, expr)

and parse_orexpr toks =
  let (toks_after_parse_andexpr, expr) = parse_andexpr toks in
    let t = lookahead toks_after_parse_andexpr in
      match t with
      |Tok_Or -> let list_after_match = (match_token toks_after_parse_andexpr Tok_Or) in
        let (toks_after_rec_call, expr_after_rec) = (parse_orexpr list_after_match) in
          (toks_after_rec_call, Or(expr, expr_after_rec))
      |_ -> (toks_after_parse_andexpr, expr)
and parse_andexpr toks =
  let (toks_after_parse_eqexpr, expr) = parse_eqexpr toks in
    let t = lookahead toks_after_parse_eqexpr in 
      match t with
      |Tok_And -> let list_after_match = (match_token toks_after_parse_eqexpr Tok_And) in
        let (toks_after_rec_call, expr_after_rec) = (parse_andexpr list_after_match) in
          (toks_after_rec_call, And(expr, expr_after_rec))
      |_ -> (toks_after_parse_eqexpr, expr)
and parse_eqexpr toks =
  let (toks_after_parse_relexpr, expr) = parse_relexpr toks in
    let t = lookahead toks_after_parse_relexpr in
      match t with
      |Tok_Equal -> let list_after_match = (match_token toks_after_parse_relexpr Tok_Equal) in
        let (toks_after_rec_call, expr_after_rec) = (parse_eqexpr list_after_match) in
          (toks_after_rec_call, Equal(expr, expr_after_rec))
      |Tok_NotEqual -> let list_after_match = (match_token toks_after_parse_relexpr Tok_NotEqual) in
        let (toks_after_rec_call, expr_after_rec) = (parse_eqexpr list_after_match) in
          (toks_after_rec_call, NotEqual(expr, expr_after_rec))
      |_ -> (toks_after_parse_relexpr, expr)
and parse_relexpr toks =
  let (toks_after_parse_addexpr, expr) = parse_addexpr toks in
    let t = lookahead toks_after_parse_addexpr in
      match t with
      |Tok_Less -> let list_after_match = (match_token toks_after_parse_addexpr Tok_Less) in
        let (toks_after_rec_call, expr_after_rec) = (parse_relexpr list_after_match) in
          (toks_after_rec_call, Less(expr, expr_after_rec))
      |Tok_Greater -> let list_after_match = (match_token toks_after_parse_addexpr Tok_Greater) in
        let (toks_after_rec_call, expr_after_rec) = (parse_relexpr list_after_match) in
          (toks_after_rec_call, Greater(expr, expr_after_rec))
      |Tok_LessEqual -> let list_after_match = (match_token toks_after_parse_addexpr Tok_LessEqual) in
        let (toks_after_rec_call, expr_after_rec) = (parse_relexpr list_after_match) in
          (toks_after_rec_call, LessEqual(expr, expr_after_rec))
      |Tok_GreaterEqual -> let list_after_match = (match_token toks_after_parse_addexpr Tok_GreaterEqual) in
        let (toks_after_rec_call, expr_after_rec) = (parse_relexpr list_after_match) in
          (toks_after_rec_call, GreaterEqual(expr, expr_after_rec))
      |_ -> (toks_after_parse_addexpr, expr)
and parse_addexpr toks =
  let (toks_after_parse_multexpr, expr) = parse_multexpr toks in
    let t = lookahead toks_after_parse_multexpr in
      match t with
      |Tok_Add -> let list_after_match = (match_token toks_after_parse_multexpr Tok_Add) in
        let (toks_after_rec_call, expr_after_rec) = (parse_addexpr list_after_match) in
          (toks_after_rec_call, Add(expr, expr_after_rec))
      |Tok_Sub -> let list_after_match = (match_token toks_after_parse_multexpr Tok_Sub) in
        let (toks_after_rec_call, expr_after_rec) = (parse_addexpr list_after_match) in
          (toks_after_rec_call, Sub(expr, expr_after_rec))
      |_ -> (toks_after_parse_multexpr, expr)
and parse_multexpr toks =
  let (toks_after_parse_powexpr, expr) = parse_powexpr toks in
    let t = lookahead toks_after_parse_powexpr in
      match t with
      |Tok_Mult -> let list_after_match = (match_token toks_after_parse_powexpr Tok_Mult) in
        let (toks_after_rec_call, expr_after_rec) = (parse_multexpr list_after_match) in
          (toks_after_rec_call, Mult(expr, expr_after_rec))
      |Tok_Div -> let list_after_match = (match_token toks_after_parse_powexpr Tok_Div) in
        let (toks_after_rec_call, expr_after_rec) = (parse_multexpr list_after_match) in
          (toks_after_rec_call, Div(expr, expr_after_rec))
      |_ -> (toks_after_parse_powexpr, expr)
and parse_powexpr toks = 
  let (toks_after_parse_unexpr, expr) = parse_unexpr toks in
    let t = lookahead toks_after_parse_unexpr in
      match t with
      |Tok_Pow -> let list_after_match = (match_token toks_after_parse_unexpr Tok_Pow) in
        let (toks_after_rec_call, expr_after_rec) = (parse_powexpr list_after_match) in
          (toks_after_rec_call, Pow(expr, expr_after_rec))
      |_ -> (toks_after_parse_unexpr, expr)
and parse_unexpr toks = 
  let t = lookahead toks in
    match t with
    |Tok_Not -> let list_after_match = (match_token toks Tok_Not) in
      let (toks_after_rec_call, expr_after_rec) = (parse_unexpr list_after_match) in
        (toks_after_rec_call, Not(expr_after_rec))
    |_ -> parse_primexpr toks
and parse_primexpr toks =
  let t = lookahead toks in
    match t with
    |Tok_Bool i -> let fin_toks = match_token toks (Tok_Bool i) in (fin_toks, (Bool i))
    |Tok_Int i -> let fin_toks = match_token toks (Tok_Int i) in (fin_toks, (Int i))
    |Tok_ID i -> let fin_toks = match_token toks (Tok_ID i) in (fin_toks, (ID i))
    |Tok_LParen -> let toks_after_match_l = match_token toks Tok_LParen in
      let (toks_after_rec_call, expr_after_rec) = (parse_expr toks_after_match_l) in
        let toks_after_match_r = match_token toks_after_rec_call Tok_RParen in
          (toks_after_match_r, expr_after_rec)
    |_ -> raise (InvalidInputException "Failed at the end of parse_expr")

(* This will parse one statement and return the tuple. The main parse_stmt method
   calls this over and over and wraps the results in the Seq() "chains" accordingly*)
let rec parse_next_stmt toks : stmt_result = 
  let t = lookahead toks in match t with
    |Tok_Int_Type -> let list_after_type = match_token toks Tok_Int_Type in
      let t1 = lookahead list_after_type in 
      (match t1 with
      |Tok_ID i -> let list_after_ID = match_token list_after_type (Tok_ID i) in
        let list_after_semi = match_token list_after_ID Tok_Semi in
          (list_after_semi, Declare(Int_Type, i))
      |_ -> raise (InvalidInputException "Failed in parse_next_stmt, while matching with Tok_Int_Type"))
    |Tok_Bool_Type -> let list_after_type = match_token toks Tok_Bool_Type in
      let t1 = lookahead list_after_type in 
      (match t1 with
      |Tok_ID i -> let list_after_ID = match_token list_after_type (Tok_ID i) in
        let list_after_semi = match_token list_after_ID Tok_Semi in
          (list_after_semi, Declare(Bool_Type, i))
      |_ -> raise (InvalidInputException "Failed in parse_next_stmt, while matching with Tok_Bool_Type"))
    |Tok_ID i -> let list_after_ID = match_token toks (Tok_ID i) in 
      let list_after_assign = match_token list_after_ID Tok_Assign in
        let (list_after_parse_expr, expr_after_parse) = parse_expr list_after_assign in
          let list_after_semi = match_token list_after_parse_expr Tok_Semi in
            (list_after_semi, Assign(i, expr_after_parse))
    |Tok_Print -> let list_after_print = match_token toks Tok_Print in
      let list_after_lparen = match_token list_after_print Tok_LParen in
        let (list_after_parse_expr, expr_after_parse) = parse_expr list_after_lparen in
          let list_after_rparen = match_token list_after_parse_expr Tok_RParen in
            let list_after_semi = match_token list_after_rparen Tok_Semi in
              (list_after_semi, Print(expr_after_parse))
    |Tok_If -> let list_after_if = match_token toks Tok_If in
      let list_after_lparen = match_token list_after_if Tok_LParen in
        let (list_after_parse_expr, expr_after_parse_expr) = parse_expr list_after_lparen in
          let list_after_rparen = match_token list_after_parse_expr Tok_RParen in
            let list_after_lbrace = match_token list_after_rparen Tok_LBrace in
              let (list_after_parse_stmt, expr_after_parse_stmt) = parse_stmt list_after_lbrace in
                let list_after_rbrace = match_token list_after_parse_stmt Tok_RBrace in
                  let else_checker = lookahead list_after_rbrace in
                  (match else_checker with
                   |Tok_Else -> let list_after_else = match_token list_after_rbrace Tok_Else in
                      let list_after_lbrace2 = match_token list_after_else Tok_LBrace in
                        let (list_after_parse_stmt2, expr_after_parse_stmt2) = parse_stmt list_after_lbrace2 in
                          let list_after_rbrace2 = match_token list_after_parse_stmt2 Tok_RBrace in
                            (list_after_rbrace2, If(expr_after_parse_expr, expr_after_parse_stmt, expr_after_parse_stmt2))
                   |_ -> (list_after_rbrace, If(expr_after_parse_expr, expr_after_parse_stmt, NoOp)))
    |Tok_For -> let list_after_for = match_token toks Tok_For in
      let list_after_lparen = match_token list_after_for Tok_LParen in
        let t1 = lookahead list_after_lparen in
        (match t1 with
         |Tok_ID i -> let list_after_ID = match_token list_after_lparen (Tok_ID i) in
            let list_after_from = match_token list_after_ID Tok_From in
              let (list_after_parse_expr, expr_after_parse_expr) = parse_expr list_after_from in
                let list_after_to = match_token list_after_parse_expr Tok_To in
                  let (list_after_parse_expr2, expr_after_parse_expr2) = parse_expr list_after_to in
                    let list_after_rparen = match_token list_after_parse_expr2 Tok_RParen in
                      let list_after_lbrace = match_token list_after_rparen Tok_LBrace in
                        let (list_after_parse_stmt, expr_after_parse_stmt) = parse_stmt list_after_lbrace in
                          let list_after_rbrace = match_token list_after_parse_stmt Tok_RBrace in
                            (list_after_rbrace, For(i, expr_after_parse_expr, expr_after_parse_expr2, expr_after_parse_stmt))
         |_ -> raise (InvalidInputException "Failed in parse_next_stmt, while matching with Tok_For"))
    |Tok_While -> let list_after_while = match_token toks Tok_While in
      let list_after_lparen = match_token list_after_while Tok_LParen in
        let (list_after_parse_expr, expr_after_parse_expr) = parse_expr list_after_lparen in
          let list_after_rparen = match_token list_after_parse_expr Tok_RParen in 
            let list_after_lbrace = match_token list_after_rparen Tok_LBrace in
              let (list_after_parse_stmt, expr_after_parse_stmt) = parse_stmt list_after_lbrace in
                let list_after_rbrace = match_token list_after_parse_stmt Tok_RBrace in
                  (list_after_rbrace, While(expr_after_parse_expr, expr_after_parse_stmt))
    |_ -> (toks, NoOp) (* This just means the next tokens aren't a statment so it's done processing *)

(* This one actually formulates the proper return value. It parses the next individual statement,
   then checks the next token in the remaining list to see if another statement needs to be
   parsed. If not, then it returns a Seq() with a NoOp on the right instead of a recursive call*)
and parse_stmt toks : stmt_result =
  let (list_after_left, left_expr) = parse_next_stmt toks in
    let t = lookahead list_after_left in
    match t with
    |Tok_Int_Type -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_Bool_Type -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_Print -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_If -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_For -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_While -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |Tok_ID i -> let (list_after_right, right_expr) = parse_stmt list_after_left in
      (list_after_right, Seq(left_expr, right_expr))
    |_ when (left_expr <> NoOp) -> (list_after_left, Seq(left_expr, NoOp))
    |_ when ((lookahead list_after_left) = Tok_RBrace)-> (list_after_left, NoOp)
    |_ -> raise (InvalidInputException "Failed in parse_stmt") (* Throws exception if rest of token list is invalid statement and has no rbrace indicating invalid statement *)

let parse_main toks : stmt =
  let list_after_type = match_token toks Tok_Int_Type in
    let list_after_main = match_token list_after_type Tok_Main in
      let list_after_lparen = match_token list_after_main Tok_LParen in
        let list_after_rparen = match_token list_after_lparen Tok_RParen in
          let list_after_lbrace = match_token list_after_rparen Tok_LBrace in
            let (list_after_parse, program_expr) = parse_stmt list_after_lbrace in
              let list_after_rbrace = match_token list_after_parse Tok_RBrace in
                let list_after_EOF = match_token list_after_rbrace EOF in
                  match list_after_EOF with
                  |[] -> program_expr
                  |_ -> raise (InvalidInputException "Failed in parse_main, tokens exist after EOF")