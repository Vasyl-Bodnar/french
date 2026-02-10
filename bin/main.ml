(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open French.Sat
open French.Parser

let usage =
  {|
french <input> [option...]
input:
  <filename>   :: File to use as input
  -f --file    :: Explicit file
  -s --string  :: String to use as input
options:
  -v --verbose :: Verbosity bool
  -t --temp    :: Starting temperature
  -i --iters   :: How many iterations to run each time
  -h --help    :: Print this message
|}

type input_opt = Str of string | File of string | NoInput

let input_opt = ref NoInput

let temp = ref 1.0

let iters = ref 100

let verbose = ref false

let anon filename = input_opt := File filename

let str s = input_opt := Str s

let spec =
  [ ("-f", Arg.String anon, "")
  ; ("--file", Arg.String anon, "")
  ; ("-s", Arg.String str, "")
  ; ("--string", Arg.String str, "")
  ; ("-t", Arg.Set_float temp, "")
  ; ("--temp", Arg.Set_float temp, "")
  ; ("-i", Arg.Set_int iters, "")
  ; ("--iters", Arg.Set_int iters, "")
  ; ("-v", Arg.Set verbose, "")
  ; ("--verbose", Arg.Set verbose, "")
  ; ("-h", Arg.Unit (fun () -> print_endline usage ; exit 0), "")
  ; ("-help", Arg.Unit (fun () -> print_endline usage ; exit 0), "")
  ; ("--help", Arg.Unit (fun () -> print_endline usage ; exit 0), "") ]

let run_anneal parser =
  if !verbose then (
    Printf.printf "Temp: %f Iters: %d\n" !temp !iters ;
    print_parser parser ) ;
  let inputs = Array.make parser.num_vars false in
  let err, iters = anneal !temp !iters inputs parser.formula in
  if err = 0 then
    Printf.printf "Output: SAT\nIterations: %d\nBest inputs: " iters
  else
    Printf.printf
      "Output: UNKNOWN, Unsatisfied Clauses: %d\nIterations: %d\nBest inputs: "
      err iters ;
  print_vars inputs

let () =
  Arg.parse spec anon usage ;
  match !input_opt with
  | Str s ->
      run_anneal @@ parse_dimacs s
  | File f ->
      let reader = open_in f in
      let s = In_channel.input_all reader in
      run_anneal @@ parse_dimacs s
  | NoInput ->
      print_endline "Requires an input string or file"
