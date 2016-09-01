open Format
open Yxml
open Term
open UnbufferedIO

type fork_result = Parent of int * Unix.file_descr | Child

let srv_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 11223)

let isa_path = "/opt/Isabelle2016/bin/isabelle_process -P \"127.0.0.1:11223\""

let pp_xml_attribute fmt (name, value) =
  fprintf fmt "%s=\"%s\"" name value

let pp_xml_attributes =
  let pp_sep fmt () = fprintf fmt " " in
  pp_print_list ~pp_sep pp_xml_attribute

let pp_xml_tree fmt tree =
  let rec pp_xml_tree_ prefix fmt = function
    | Yxml.XML.Elem ((name, attributes), children) ->
      fprintf fmt "%s<%s %a>%a</%s>@," prefix name
        pp_xml_attributes attributes
        (pp_print_list (pp_xml_tree_ (String.concat "" [" "; prefix]))) children
        name;
      ()
    | Yxml.XML.Text str ->
      fprintf fmt "%s%s@," prefix str;
      ()
  in
  fprintf fmt "%a" (pp_xml_tree_ "") tree

let pp_xml_body = pp_print_list pp_xml_tree


let read_chunk isaout =
  try
    let len_str = input_line isaout in
    (*    fprintf err_formatter "recieved length: %s" len_str; *)
    let len = int_of_string len_str in
    let buf = Bytes.create len in
    let len_read = Unix.read isaout buf 0 len in
    if (len_read <> len) then
      failwith "Couldn't read expected number of chars from network.";
    let xml = Yxml.YXML.parse_body (Bytes.to_string buf)  in
    fprintf err_formatter "%a@." pp_xml_body xml;
    ()
  with
  | Unix.Unix_error (error, f, param) ->
    fprintf err_formatter "Isabelle - Unix error in %s: %s@." f
      (Unix.error_message error);
  | Failure str ->
    failwith ("Isabelle communcation failed with: " ^ str)


let fork_child child_handler =
  let (pipe_reader, pipe_writer) = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close pipe_writer;
    child_handler pipe_reader ();
    Unix.close pipe_reader;
    Child
  | pid ->
    Unix.close pipe_reader;
    Parent (pid, pipe_writer)


let handle_commands fd cond =
  match input_line fd with
  | "shutdown" ->
    fprintf err_formatter "Shutdown command!@.";
    cond := false;
    ()
  | str ->
    fprintf err_formatter "Unkown command %s@." str;
    ()

let isabelle_handler pipe_reader () =
  fprintf err_formatter "Starting up isabelle@.";
  let env = Unix.environment () in
  let (isaout, isain, isaerr) = Unix.open_process_full isa_path env in
  let isaout_descr = Unix.descr_of_in_channel isaout in
  begin
    try
      let condition = ref true in
      while (!condition) do
        fprintf err_formatter "isabelle alive@.";
        match
          Unix.select [pipe_reader; isaout_descr] [] [] 1.0
        with
        | ([], _, _) ->
          ()
        | ([d1], _, _) when d1 = pipe_reader ->
          handle_commands pipe_reader condition;
          ()
        | ([d1], _, _) when d1 = isaout_descr ->
          let _cliout = Pervasives.input_char isaout in
          ()
        | ([_; _], _, _)  ->
          handle_commands pipe_reader condition;
          let _cliout = Pervasives.input_char isaout in
          ()
        | _ -> failwith "Implementation error!"
      done;
      ()
    with
    | End_of_file ->
      fprintf err_formatter "EOF from Isabelle process@.";
      ()
  end;
  fprintf err_formatter "Closing Isabelle process@.";
  let status = Unix.close_process_full (isaout, isain, isaerr) in
  begin
    match status with
    | Unix.WEXITED code ->
      fprintf err_formatter "Isabelle exited with code %d@." code
    | Unix.WSIGNALED code ->
      fprintf err_formatter "Isabelle got signaled with code %d@." code
    | Unix.WSTOPPED code ->
      fprintf err_formatter "Isabelle stopped with code %d@." code
  end;
  ()

let run_server pipe_reader () =
  fprintf err_formatter "Starting up tcp connector@.";
  let sock = Unix.socket (Unix.domain_of_sockaddr srv_addr) Unix. SOCK_STREAM 0 in
  Unix.bind sock srv_addr ;
  Unix.listen sock 3;
  let (clisock, cliaddr) = Unix.accept sock in
  let condition = ref true in
  while (!condition) do
    fprintf err_formatter "server alive@.";
    match
      Unix.select [pipe_reader; clisock] [] [] 1.0
    with
    | ([], _, _) ->
      ()
    | ([d1], _, _) when d1 = pipe_reader ->
      handle_commands pipe_reader condition;
      ()
    | ([d1], _, _) when d1 = clisock ->
      read_chunk clisock;
      ()
    | ([_; _], _, _)  ->
      handle_commands pipe_reader condition;
      read_chunk clisock;
      ()
    | _ -> failwith "Implementation error!"
  done;
  fprintf err_formatter "Server recieved shutdown message!";
  Unix.close clisock;
  ()

;;

let () =
  try
    match fork_child run_server with
    | Child ->
      fprintf err_formatter "Server process finished!@.";
      ()
    | Parent (srv_pid, srv_pipe) ->
      begin
        match fork_child isabelle_handler with
        | Child ->
          fprintf err_formatter "Isabelle process finished!@.";
          ()
        | Parent (isa_pid, isa_pipe) ->
          (*
          let isa_chan = Unix.out_channel_of_descr isa_pipe in
          let srv_chan = Unix.out_channel_of_descr isa_pipe in
          *)
          fprintf err_formatter "Controller up, hit enter to quit.@.";
          let _ = read_line () in
          (* close server process *)
          fprintf err_formatter "Sending halt to child processes...@.";
          let buf = Bytes.of_string "shutdown\n" in
          ignore ( Unix.write srv_pipe buf 0 (Bytes.length buf) );
          ignore ( Unix.write isa_pipe buf 0 (Bytes.length buf) );
          fprintf err_formatter "Waiting for child processes to finish...@.";
          let _ = Unix.waitpid [] srv_pid in
          let _ = Unix.waitpid [] isa_pid in
          Unix.close isa_pipe;
          Unix.close srv_pipe;
          ()
      end
  with
  | Unix.Unix_error (error, f, param) ->
    fprintf err_formatter "Unix error in %s: %s@." f (Unix.error_message error);
