(*** Untyped XML trees and typed representation of ML values ***)

module type XML_Data_Ops =
sig
  type 'a a
  type 'a t
  type 'a v
  val int_atom: int a
  val bool_atom: bool a
  val unit_atom: unit a
  val properties: (string * string) list t
  val string: string t
  val int: int t
  val bool: bool t
  val unit: unit t
  val pair: 'a t -> 'b t -> ('a * 'b) t
  val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list: 'a t -> 'a list t
  val option: 'a t -> 'a option t
  val variant: 'a v list -> 'a t
end

module type XML =
sig
  type attributes = (string * string) list
  type tree = Elem of ((string * attributes) * tree list) | Text of string
  type body = tree list
  exception XML_Atom of string
  exception XML_Body of tree list
  module Encode: XML_Data_Ops with
    type 'a a = 'a -> string and
    type 'a t = 'a -> body and
    type 'a v = 'a -> string list * body
  module Decode: XML_Data_Ops with
    type 'a a = string -> 'a and
    type 'a t = body -> 'a and
    type 'a v = string list * body -> 'a
end

module XML: XML =
struct

type attributes = (string * string) list
type tree = Elem of ((string * attributes) * tree list) | Text of string
type body = tree list


let map_index f =
  let rec mapp i = function
    | [] -> []
    | x :: xs -> f (i, x) :: mapp (i + 1) xs
  in mapp 0

exception XML_Atom of string
exception XML_Body of tree list


module Encode =
struct

type 'a a = 'a -> string
type 'a t = 'a -> body
type 'a v = 'a -> string list * body


(* atomic values *)

let int_atom = string_of_int

let bool_atom = function false -> "0" | true -> "1"

let unit_atom () = ""


(* structural nodes *)

let node ts = Elem ((":", []), ts)

let vector = map_index (fun (i, x) -> (int_atom i, x))

let tagged (tag, (xs, ts)) = Elem ((int_atom tag, vector xs), ts)


(* representation of standard types *)

let properties props = [Elem ((":", props), [])]

let string = function "" -> [] | s -> [Text s]

let int i = string (int_atom i)

let bool b = string (bool_atom b)

let unit () = string (unit_atom ())

let pair f g (x, y) = [node (f x); node (g y)]

let triple f g h (x, y, z) = [node (f x); node (g y); node (h z)]

let list f xs = List.map (fun x -> node (f x)) xs

let option f = function None -> [] | Some x -> [node (f x)]

let variant fns x =
  let rec get_index i = function
    | [] -> raise (Failure "XML.Encode.variant")
    | f :: fs -> try (i, f x) with Match_failure _ -> get_index (i + 1) fs
  in [tagged (get_index 0 fns)]

end


module Decode =
struct

type 'a a = string -> 'a
type 'a t = body -> 'a
type 'a v = string list * body -> 'a


(* atomic values *)

let int_atom s =
  try int_of_string s
    with Invalid_argument _ -> raise (XML_Atom s)

let bool_atom = function
  | "0" -> false
  | "1" -> true
  | s -> raise (XML_Atom s)

let unit_atom s =
  if s = "" then () else raise (XML_Atom s)


(* structural nodes *)

let node = function
  | Elem ((":", []), ts) -> ts
  | t -> raise (XML_Body [t])

let vector =
  map_index (function (i, (a, x)) -> if int_atom a = i then x else raise (XML_Atom a))

let tagged = function
  | Elem ((name, atts), ts) -> (int_atom name, (vector atts, ts))
  | t -> raise (XML_Body [t])


(* representation of standard types *)

let properties = function
  | [Elem ((":", props), [])] -> props
  | ts -> raise (XML_Body ts)

let string = function
  | [] -> ""
  | [Text s] -> s
  | ts -> raise (XML_Body ts)

let int ts = int_atom (string ts)

let bool ts = bool_atom (string ts)

let unit ts = unit_atom (string ts)

let pair f g = function
  | [t1; t2] -> (f (node t1), g (node t2))
  | ts -> raise (XML_Body ts)

let triple f g h = function
  | [t1; t2; t3] -> (f (node t1), g (node t2), h (node t3))
  | ts -> raise (XML_Body ts)

let list f = List.map (fun t -> f (node t))

let option f = function
  | [] -> None
  | [t] -> Some (f (node t))
  | ts -> raise (XML_Body ts)

let variant fs = function
  | [t] ->
      let (tag, (xs, ts)) = tagged t in
      let f = try List.nth fs tag with Invalid_argument _ -> raise (XML_Body [t]) in
      f (xs, ts)
  | ts -> raise (XML_Body ts)

end

end



(*** YXML transfer syntax ***)

(*
Efficient text representation of XML trees using extra characters X
and Y -- no escaping, may nest marked text verbatim.  Suitable for
direct inlining into plain text.

Markup <elem att="val" ...>...body...</elem> is encoded as:

  X Y name Y att=val ... X
  ...
  body
  ...
  X Y X
*)

module type YXML =
sig
  val char_X: char
  val char_Y: char
  val no_output: string * string
  val output_markup: string * XML.attributes -> string * string
  val string_of_body: XML.body -> string
  val string_of: XML.tree -> string
  val parse_body: string -> XML.body
  val parse: string -> XML.tree
end

module YXML: YXML =
struct

(* markers *)

let char_X = '\005'
let char_Y = '\006'

let str_X = String.make 1 char_X
let str_Y = String.make 1 char_Y

let str_XY = str_X ^ str_Y
let str_XYX = str_XY ^ str_X

let detect s = String.contains s char_X || String.contains s char_Y


(* ML basics *)

let (|>) x f = f x
let (@>) f g x = g (f x)

let rec fold f list y =
  match list with
    [] -> y
  | x :: xs -> fold f xs (f x y)


(* output *)

let implode = String.concat ""
let content xs = implode (List.rev xs)
let add x xs = if x = "" then xs else x :: xs

let no_output = ("", "")

let output_markup (name, atts) =
  if name = "" then no_output
  else
    (str_XY ^ name ^ implode (List.map (fun (a, x) -> str_Y ^ a ^ "=" ^ x) atts) ^ str_X, str_XYX)

let string_of_body body =
  let attrib (a, x) = add str_Y @> add a @> add "=" @> add x in
  let rec tree = function
    | XML.Elem ((name, atts), ts) ->
        add str_XY @> add name @> fold attrib atts @> add str_X @>
        trees ts @>
        add str_XYX
    | XML.Text s -> add s
  and trees ts = fold tree ts
  in content (trees body [])

let string_of tree = string_of_body [tree]



(** parsing *)

(* split *)

let split fields sep str =
  let cons i n result =
    if i = 0 && n = String.length str && n > 0 then str :: result
    else if n > 0 then String.sub str i n :: result
    else if fields then "" :: result
    else result
  in
  let rec explode i result =
    let j = try String.index_from str i sep with Not_found -> -1 in
      if j >= 0 then explode (j + 1) (cons i (j - i) result)
      else List.rev (cons i (String.length str - i) result)
  in explode 0 []


(* parse *)

let err msg = raise (Failure ("Malformed YXML: " ^ msg))
let err_attribute () = err "bad attribute"
let err_element () = err "bad element"
let err_unbalanced name =
  if name = "" then err "unbalanced element"
  else err ("unbalanced element \"" ^ name ^ "\"")

let parse_attrib s =
  try
    let i = String.index s '=' in
    let _ = if i = 0 then err_attribute () in
    let j = i + 1 in
      (String.sub s 0 i, String.sub s j (String.length s - j))
  with Not_found -> err_attribute ()

let parse_body source =

  (* stack operations *)

  let add x ((elem, body) :: pending) = (elem, x :: body) :: pending
  in

  let push name atts pending =
    if name = "" then err_element ()
    else ((name, atts), []) :: pending
  in

  let pop (((name, _) as markup, body) :: pending) =
    if name = "" then err_unbalanced ""
    else add (XML.Elem (markup, List.rev body)) pending
  in

  (* parse chunks *)

  let chunks = split false char_X source |> List.map (split true char_Y) in

  let parse_chunk = function
    | [""; ""] -> pop
    | ("" :: name :: atts) -> push name (List.map parse_attrib atts)
    | txts -> fold (fun s -> add (XML.Text s)) txts
  in
  match fold parse_chunk chunks [(("", []), [])] with
  | [(("", _), result)] -> List.rev result
  | ((name, _), _) :: _ -> err_unbalanced name

let parse source =
  match parse_body source with
  | [result] -> result
  | [] -> XML.Text ""
  | _ -> err "multiple results"

end

