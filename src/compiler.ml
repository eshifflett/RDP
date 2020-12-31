open Parser
open Eval
open Utils
open EvalUtils

let () = 
    let input = Sys.argv.(1) in 
        let tokens = tokenize_from_file input in
            let ast = parse_main tokens in
                let env = eval_stmt [] ast in
                    print_eval_env_report env;;
                    ()
        


