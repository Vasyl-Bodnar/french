(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open French.Sat

let () =
  Random.self_init () ;
  let inputs = Array.make 10 false in
  let err, iters =
    anneal 1.0 100 inputs
      [ [1; -2; 3]
      ; [-4; 5; -6]
      ; [7; 8; -1]
      ; [-9; 10; 2]
      ; [3; 5; 7]
      ; [-8; -1; 4]
      ; [2; -10; 6]
      ; [9; -3; -5]
      ; [1; 4; 7]
      ; [-2; -6; -8]
      ; [10; -9; 5]
      ; [3; -4; 1]
      ; [6; 7; 8]
      ; [-10; 2; 4]
      ; [5; -1; -3]
      ; [9; 6; -2] ]
  in
  Printf.printf "Output: %d\nIterations: %d\nBest inputs: " err iters ;
  Array.iter
    (fun b ->
      if b then print_int 1 else print_int 0 ;
      print_string " " )
    inputs
