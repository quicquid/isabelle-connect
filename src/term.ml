(*** Example: Isabelle terms ***)
open Yxml

module Term =
struct

type indexname = string * int
type sort = string list
type arity = string * sort list * sort
type typ =
  Type of string * typ list |
  TFree of string * sort |
  TVar of indexname * sort
type term =
  Const of string * typ |
  Free of string * typ |
  Var of indexname * typ |
  Bound of int |
  Abs of string * typ * term |
  App of term * term

module type Term_XML_Ops =
sig
  type 'a t
  val sort: sort t
  val typ: typ t
  val term: term t
end

module Encode: Term_XML_Ops with type 'a t = 'a -> XML.body =
struct

include XML.Encode

let sort = list string

let rec typ ty = variant
 [(function Type (a, b) -> ([a], list typ b));
  (function TFree (a, b) -> ([a], sort b));
  (function TVar ((a, b), c) -> ([a; int_atom b], sort c))] ty

let rec term tm = variant
 [(function Const (a, b) -> ([a], typ b));
  (function Free (a, b) -> ([a], typ b));
  (function Var ((a, b), c) -> ([a; int_atom b], typ c));
  (function Bound a -> ([int_atom a], []));
  (function Abs (a, b, c) -> ([a], pair typ term (b, c)));
  (function App (a, b) -> ([], pair term term (a, b)))] tm

end

module Decode: Term_XML_Ops with type 'a t = XML.body -> 'a =
struct

include XML.Decode

let sort = list string

let rec typ ty = variant
 [(function ([a], b) -> Type (a, list typ b));
  (function ([a], b) -> TFree (a, sort b));
  (function ([a; b], c) -> TVar ((a, int_atom b), sort c))] ty

let rec term t = variant
 [(function ([a], b) -> Const (a, typ b));
  (function ([a], b) -> Free (a, typ b));
  (function ([a; b], c) -> Var ((a, int_atom b), typ c));
  (function ([a], []) -> Bound (int_atom a));
  (function ([a], b) -> let (c, d) = pair typ term b in Abs (a, c, d));
  (function ([], a) -> let (b, c) = pair term term a in App (b, c))] t

end

end
