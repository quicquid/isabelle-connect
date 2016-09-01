open Unix

let timeit e =
  let start = Unix.gettimeofday () in
  let result = e () in
  let stop = Unix.gettimeofday () in
  let _ = print_float (stop -. start) in result

let read_file name =
  let ch = open_in name in
  let n = in_channel_length ch in
  let s = String.create n in
  let _ = really_input ch s 0 n in
  let _ = close_in ch in s
