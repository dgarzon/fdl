open Parser
open Scanner
open Codegen

let _ =
    let code = gen_prog (* sast globals *)
    in print_endline code
