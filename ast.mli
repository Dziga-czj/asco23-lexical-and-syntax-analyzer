type ast =
  | Group of ast
  | Plus of ast * ast
  | Times of ast * ast
  | Cst of int
  | Id of string

val affiche : ast -> unit
