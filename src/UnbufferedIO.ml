open Unix

exception UnbufferedIOFailed of string

let input_char fd =
  let curr_buff = Bytes.create 16 in
  match read fd curr_buff 0 1 with
  | 1 ->
    Bytes.get curr_buff 0
  | _ -> 
    raise (UnbufferedIOFailed "input_char: couldn't read the character!")

let input_line fd =
  let buf = Buffer.create 100 in
  let rec input_line_ f =
    match input_char f with
    | '\n' ->
      ()
    | c ->
      Buffer.add_char buf c;
      input_line_ f
  in
  input_line_ fd;
  Buffer.contents buf
