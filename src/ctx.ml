(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Syntax

(* ---------------------------------------------------------------------- *)
(* Context management *)
(* ---------------------------------------------------------------------- *)

(* Contexts of type 'a *)
type 'a ctx = (var_info * 'a) list
type context = ty ctx

let length ctx = List.length ctx
let empty_context = []

(* Return a binding if it exists. Let the caller handle the error *)
let rec lookup_var id ctx =
  match ctx with
      []                -> None
    | (var, value) :: l ->
      if var.v_name = id then
        Some (var, value)
      else
        lookup_var id l

(* Shifting of v_names, this is mostly for debug and 
  corresponds to a new abstraction *)
let varctx_var_shift n d ctx =
  List.map (fun (v, ty) -> (var_shift n d v, ty)) ctx

(* Extend the context with a new variable binding. 
   We just shift term variables *)
let extend_var id bi ctx =
  let n_var = { 
    v_name  = id;
    v_index = 0;
    v_size  = (length ctx) + 1;
  } in
  let s_ctx = varctx_var_shift 0 1 ctx in
    (n_var, bi) :: s_ctx 

let remove_first_var ctx =
  if ctx = empty_context then 
    ctx
  else
    let s_ctx = varctx_var_shift 0 (-1) ctx in List.tl s_ctx

let rec split_below ctx n =
    if n = 0 then List.tl ctx
    else 
        match ctx with
        | [] -> []
        | _ :: tail ->
            split_below tail (n-1)

let split_above ctx n = List.rev (split_below (List.rev ctx) n)

let remove_nth_var n ctx =
  if ctx = empty_context then
    ctx
  else
    let s_ctx = varctx_var_shift 0 (-1) (split_below ctx n) in
      split_above ctx n   @ s_ctx

(* Remove vars from ctx, l is list of indices *)
let rec reduce_ctx l ctx2 = 
  if ctx2 = empty_context then 
    ctx2
  else
    match ctx2 with
    | [] -> ctx2
    | (var, value) :: l ->  
            reduce_ctx l (remove_nth_var var.v_index ctx2)

(* Accessing the variable in the context *)
let access_var ctx i = List.nth ctx i
