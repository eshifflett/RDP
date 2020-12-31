open SmallCTypes
open EvalUtils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(* Useful functions for operating on environments, pulled from Lec 17 *)
(* Adds mapping from x to v into environment env *)
let rec extend env x v = 
  match env with
  |[] -> [(x, v)]
  |(a, b)::t when a = x -> (x, v)::t
  |(a, b)::t -> (a, b)::(extend t x v)

(* Looks for value in environment env with key x *)
let rec lookup env x =
  match env with
  |[] -> raise (DeclareError "No match in environment")
  |(y, z)::env1 -> if x = y then z else (lookup env1 x)

(* Checks if the input value x is an Int_Val and returns its value, otherwise throws a TypeError *)
let int_check x =
  match x with
  |Int_Val (i) -> i
  |_ -> raise (TypeError "Type error in int_check")

(* Checks if the input value x is an Int_Bool and returns its value, otherwise throws a TypeError *)
let bool_check x =
  match x with
  |Bool_Val (i) -> i
  |_ -> raise (TypeError "Type error in bool_check")

(* Checks if the ID given in tuple x is already in the environment env *)
let rec mem_of_env env x =
  match env with
  |[] -> false
  |(c, d)::t -> if (x = c) then true else (mem_of_env t x)

(* Power function for integers *)
let rec pow x y = 
    match y with
    |0 -> 1
    |_ when y < 0 -> failwith "negative power"
    |_ -> x * pow x (y - 1)

let rec eval_expr env t =
  match t with
  |Int (i) -> (Int_Val i)
  |Bool (i) -> (Bool_Val i)
  |ID (i) -> (lookup env i)
  |Add (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 + n4) in
              (Int_Val n5)
  |Sub (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 - n4) in
              (Int_Val n5)
  |Mult (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 * n4) in
              (Int_Val n5)
  |Div (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let numer = (int_check n1) in
          let denom = (int_check n2) in
            if (denom = 0) then (raise (DivByZeroError)) else (Int_Val (numer / denom))
  |Pow (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let base = (int_check n1) in
          let raise = (int_check n2) in
            let n3 = (pow base raise) in
              (Int_Val n3)
  |Or (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (bool_check n1) in
          let n4 = (bool_check n2) in
            let n5 = (n3 || n4) in
              (Bool_Val n5)
  |And (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (bool_check n1) in
          let n4 = (bool_check n2) in
            let n5 = (n3 && n4) in
              (Bool_Val n5)
  |Not (x) -> 
    let n1 = (eval_expr env x) in
      let n2 = (bool_check n1) in
        let n3 = (not (n2)) in
          (Bool_Val n3)
  |Greater (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 > n4) in
              (Bool_Val n5)
  |Less (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 < n4) in
              (Bool_Val n5)
  |GreaterEqual (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 >= n4) in
              (Bool_Val n5)
  |LessEqual (x, y) -> 
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        let n3 = (int_check n1) in
          let n4 = (int_check n2) in
            let n5 = (n3 <= n4) in
              (Bool_Val n5)
  |Equal (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        (match n1 with
         |Int_Val (z) -> let n3 = (int_check n2) in let n4 = (z = n3) in (Bool_Val n4)
         |Bool_Val (z) -> let n3 = (bool_check n2) in let n4 = (z = n3) in (Bool_Val n4)
        )
  |NotEqual (x, y) ->
    let n1 = (eval_expr env x) in
      let n2 = (eval_expr env y) in
        (match n1 with
         |Int_Val (z) -> let n3 = (int_check n2) in let n4 = (z <> n3) in (Bool_Val n4)
         |Bool_Val (z) -> let n3 = (bool_check n2) in let n4 = (z <> n3) in (Bool_Val n4)
        )

let rec eval_stmt env s =
  match s with
  |NoOp -> env
  |Seq (x, y) ->
    let env1 = (eval_stmt env x) in
      let env2 = (eval_stmt env1 y) in
        env2
  |Declare (typ, str) -> 
    if (mem_of_env env str) then raise (DeclareError "var exists in environment") else
      (match typ with
       |Int_Type -> let a1 = (extend env str (Int_Val 0)) in a1
       |Bool_Type -> let a1 = (extend env str (Bool_Val false)) in a1)
  |Assign (var, expr) ->
    let a1 = lookup env var in
      (match a1 with
      |Int_Val (i) ->
        let a2 = eval_expr env expr in
          let _ = int_check a2 in
            extend env var a2
      |Bool_Val (i) ->
        let a2 = eval_expr env expr in
          let _ = bool_check a2 in
            extend env var a2)
  |If (guard, t, f) -> 
    let a1 = (eval_expr env guard) in
      let g_val = (bool_check a1) in
        if (g_val) then (eval_stmt env t) else (eval_stmt env f)
  |While (guard, st) ->
    let a1 = (eval_expr env guard) in
      let g_val = (bool_check a1) in
        if (not g_val) then env else
          let env1 = (eval_stmt env st) in
            eval_stmt env1 (While (guard, st))
  |For (id, exp1, exp2, guts) ->
    let e1 = eval_expr env exp1 in
      let e2 = eval_expr env exp2 in
        let n1 = int_check e1 in
          let n2 = int_check e2 in
            if (n1 < n2) then
              let env1 = extend env id e1 in
                let env2 = eval_stmt env1 guts in
                  let a1 = lookup env2 id in
                    let a2 = int_check a1 in
                      let env3 = extend env2 id (Int_Val(a2 + 1)) in
                        eval_stmt env3 (For(id, Int(a2 + 1), Int(n2), guts))
            else if (n1 = n2) then
              let env1 = extend env id e1 in
                let env2 = eval_stmt env1 guts in
                  env2
            else env
  |Print (expr) ->
    let a1 = eval_expr env expr in
      match a1 with
      |Int_Val (i) -> print_output_int i; print_output_newline(); env
      |Bool_Val (i) -> print_output_bool i; print_output_newline(); env