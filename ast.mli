type ast =
  | Group of ast
  | Plus of ast * ast
  | Times of ast * ast
  | Cst_int of int
  | Cst_float of float
  | Cst_bool of bool
  | Id of string

val affiche : ast -> unit
