open Format
open Yxml
open Term

type fork_result = Parent of int * Unix.file_descr | Child

let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 11223)

let isa_path = "/opt/Isabelle2016/bin/isabelle_process -P \"127.0.0.1:11223\""

let pp_xml_tree fmt tree =
  let rec pp_xml_tree_ prefix fmt = function
    | Yxml.XML.Elem ((name, attributes), children) ->
      fprintf fmt "%s<%s >%a</%s>@," prefix name
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
    really_input isaout buf 0 len;
    let xml = Yxml.YXML.parse_body (Bytes.to_string buf)  in
    fprintf err_formatter "%a@." pp_xml_body xml;
    ()
  with
  | Unix.Unix_error (error, f, param) ->
    fprintf err_formatter "Isabelle - Unix error in %s: %s@." f (Unix.error_message error);
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
    

let isabelle_handler pipe_reader () =
  fprintf err_formatter "Starting up isabelle@.";
  let env = Unix.environment () in
  let (isaout, isain, isaerr) = Unix.open_process_full isa_path env in
  let isaout_descr = Unix.descr_of_in_channel isaout in
  let pipe_reader_chan = Unix.in_channel_of_descr pipe_reader in
  begin
    try
      let condition = ref true in
      while (!condition) do
        match
          Unix.select [pipe_reader; isaout_descr] [] [] 1.0
        with
        | ([], _, _) ->
          ()
        | ([d1], _, _) when d1 = pipe_reader ->
          condition := input_value pipe_reader_chan;
          ()
        | ([d1], _, _) when d1 = isaout_descr ->
          let _cliout = input_char isaout in
          ()
        | ([_; _], _, _)  ->
          condition := input_value pipe_reader_chan;
          let _cliout = input_char isaout in
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

let run_server () =
  fprintf err_formatter "Starting up...@.";
  let sock_fd = Unix.socket (Unix.domain_of_sockaddr addr) Unix. SOCK_STREAM 0 in
  ()

;;

let () =
  try
    match fork_child isabelle_handler with
    | Child ->
      fprintf err_formatter "Isabelle process finished!@.";
      ()
    | Parent (pid, isa_pipe) ->
      let isa_chan = Unix.out_channel_of_descr isa_pipe in
      fprintf err_formatter "Controller up, hit enter to quit.@.";
      let _ = read_line () in
      output_value isa_chan false;
      Unix.close isa_pipe;
      let _ = Unix.waitpid [] pid in
      ()
  with
  | Unix.Unix_error (error, f, param) ->
    fprintf err_formatter "Unix error in %s: %s@." f (Unix.error_message error);
