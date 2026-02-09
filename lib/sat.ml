(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
type clauses = int list list

let print_clauses (clauses : clauses) =
  List.map (fun x -> List.map string_of_int x |> String.concat ", ") clauses
  |> String.concat " ^ " |> print_endline

let eval_err (inputs : bool array) (clauses : clauses) =
  List.fold_left
    (fun acc clause ->
      if
        List.exists
          (fun input ->
            (input > 0 && inputs.(input - 1))
            || (input < 0 && not inputs.(-input - 1)) )
          clause
      then acc
      else acc + 1 )
    0 clauses

let anneal (temp : float) (iterations : int) (inputs : bool array)
    (clauses : clauses) =
  let rec inn temp iter all_iters err =
    if err = 0 then (err, all_iters)
    else if temp < 0.01 then (err, all_iters)
    else if iter = 0 then inn (temp *. 0.85) iterations all_iters err
    else
      let i = Random.int (Array.length inputs) in
      inputs.(i) <- not inputs.(i) ;
      let new_err = eval_err inputs clauses in
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
