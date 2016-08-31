open Format
open Yxml
open Term

let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 11223)

;;

let () =
  fprintf std_formatter "Starting up...@,";
  let sock = Unix.socket (Unix.domain_of_sockaddr addr) Unix. SOCK_STREAM 0 in
  
  ()
    
