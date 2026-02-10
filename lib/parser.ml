(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Sat

type state =
  | Comment
  | Header
  | HeaderC
  | HeaderCN
  | HeaderCNF
  | HeaderVars
  | HeaderClauses
  | ClauseInit
  | VarInit
  | VarNeg
  | Var
  | Init

type dimacs_parser =
  {state: state; formula: formula; num_clauses: int; num_vars: int}

exception Parse of string

exception Impossible

(* State machine parser for DIMACS CNF
   Note that this is technically a superset
   of a typical DIMACS CNF parser *)
let parse_dimacs s =
  String.fold_left
    (fun parser c ->
      match parser.state with
      | Init -> (
        match c with
        | 'c' ->
            {parser with state= Comment}
        | 'p' ->
            {parser with state= Header}
        | _ ->
            parser )
      | Header -> (
        match c with 'c' -> {parser with state= HeaderC} | _ -> parser )
      | HeaderC -> (
        match c with
        | 'n' ->
            {parser with state= HeaderCN}
        | _ ->
            raise (Parse "Expected p cnf") )
      | HeaderCN -> (
        match c with
        | 'f' ->
            {parser with state= HeaderCNF}
        | _ ->
            raise (Parse "Expected p cnf") )
      | HeaderCNF -> (
        match c with
        | '0' .. '9' ->
            { parser with
              state= HeaderVars
            ; num_vars= int_of_char c - int_of_char '0' }
        | _ ->
            parser )
      | HeaderVars -> (
        match c with
        | '0' .. '9' ->
            { parser with
              state= HeaderVars
            ; num_vars=
                (parser.num_vars * 10) + (int_of_char c - int_of_char '0') }
        | _ ->
            {parser with state= HeaderClauses} )
      | HeaderClauses -> (
        match c with
        | '0' .. '9' ->
            { parser with
              state= HeaderClauses
            ; num_clauses=
                (parser.num_clauses * 10) + (int_of_char c - int_of_char '0') }
        | _ ->
            {parser with state= ClauseInit} )
      | ClauseInit -> (
        match c with
        | '1' .. '9' ->
            { parser with
              state= Var
            ; formula= [int_of_char c - int_of_char '0'] :: parser.formula }
        | '-' ->
            {parser with state= VarNeg; formula= [0] :: parser.formula}
        | _ ->
            parser )
      | VarInit -> (
        match c with
        | '0' ->
            {parser with state= ClauseInit}
        | '1' .. '9' ->
            { parser with
              state= Var
            ; formula=
                ( match parser.formula with
                | clause :: rest ->
                    ((int_of_char c - int_of_char '0') :: clause) :: rest
                | _ ->
                    raise Impossible ) }
        | '-' ->
            { parser with
              state= VarNeg
            ; formula=
                ( match parser.formula with
                | clause :: rest ->
                    (0 :: clause) :: rest
                | _ ->
                    raise Impossible ) }
        | _ ->
            parser )
      | Var -> (
        match c with
        | '0' .. '9' ->
            { parser with
              state= Var
            ; formula=
                ( match parser.formula with
                | (var :: clause) :: rest ->
                    (((var * 10) + (int_of_char c - int_of_char '0')) :: clause)
                    :: rest
                | _ ->
                    raise Impossible ) }
        | _ ->
            {parser with state= VarInit} )
      | VarNeg -> (
        match c with
        | '0' .. '9' ->
            { parser with
              state= VarNeg
            ; formula=
                ( match parser.formula with
                | (var :: clause) :: rest ->
                    (((var * 10) - (int_of_char c - int_of_char '0')) :: clause)
                    :: rest
                | _ ->
                    raise Impossible ) }
        | _ ->
            {parser with state= VarInit} )
      | Comment -> (
        match c with '\n' -> {parser with state= Init} | _ -> parser ) )
    {state= Init; formula= []; num_clauses= 0; num_vars= 0}
    s

let print_state state =
  match state with
  | Comment ->
      print_string "Comment"
  | Header ->
      print_string "Header"
  | HeaderC ->
      print_string "HeaderC"
  | HeaderCN ->
      print_string "HeaderCN"
  | HeaderCNF ->
      print_string "HeaderCNF"
  | HeaderVars ->
      print_string "HeaderVars"
  | HeaderClauses ->
      print_string "HeaderClauses"
  | ClauseInit ->
      print_string "ClauseInit"
  | VarInit ->
      print_string "VarInit"
  | VarNeg ->
      print_string "VarNeg"
  | Var ->
      print_string "Var"
  | Init ->
      print_string "Init"

let print_parser parser =
  print_string "Parser: state: " ;
  print_state parser.state ;
  Printf.printf " num_clauses: %d num_vars: %d formula:\n" parser.num_clauses
    parser.num_vars ;
  print_formula parser.formula
