(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
type formula = int list list

let eval_err (vars : bool array) (formula : formula) =
  List.fold_left
    (fun acc clause ->
      if
        List.exists
          (fun input ->
            (input > 0 && vars.(input - 1))
            || (input < 0 && not vars.(-input - 1)) )
          clause
      then acc
      else acc + 1 )
    0 formula

(* TODO: Consider exploring cost in a subset (Only for clauses that are affected by the flip) *)
let anneal (temp : float) (iterations : int) (inputs : bool array)
    (formula : formula) =
  let rec inn temp iter all_iters err =
    if err = 0 then (err, all_iters)
    else if temp < 0.01 then (err, all_iters)
    else if iter = 0 then inn (temp *. 0.85) iterations all_iters err
    else
      let i = Random.int (Array.length inputs) in
      inputs.(i) <- not inputs.(i) ;
      let new_err = eval_err inputs formula in
      if new_err > err then
        if
          Float.exp (float_of_int (-(new_err - err)) /. temp) > Random.float 1.0
        then inn temp (iter - 1) (all_iters + 1) new_err
        else (
          inputs.(i) <- not inputs.(i) ;
          inn temp (iter - 1) (all_iters + 1) err )
      else inn temp (iter - 1) (all_iters + 1) new_err
  in
  inn temp iterations 0 Int.max_int

let print_vars (vars : bool array) =
  Array.iter
    (fun b ->
      if b then print_int 1 else print_int 0 ;
      print_string " " )
    vars

let print_formula (formula : formula) =
  List.map (fun x -> List.map string_of_int x |> String.concat ", ") formula
  |> String.concat " ^\n" |> print_endline
