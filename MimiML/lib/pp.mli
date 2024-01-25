open Ast
open Anf
open Format

val show_prim_ty : Typing.prim_ty -> tag
val pp_ty : formatter -> Typing.ty -> unit
val pp_scheme : formatter -> Typing.scheme -> unit
val pp_expr : formatter -> expr -> unit
val pp_anf_prog : formatter -> (tag * tag list * aprogram) list * aprogram -> unit
