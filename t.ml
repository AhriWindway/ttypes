type ('i, 'o) test_data = {
  input: 'i;
  output: 'o
};;

let test name ~tester ?(compare = (=)) ~string_of data =
  let str = string_of_int in
  let rec run (n, ok) =
    match Stream.peek data with
    | None ->
      print_endline ("### Testing of <" ^ name ^ ">: " ^ (str ok) ^ "/" ^ (str n) ^ "\n");
      (n, ok);
    | Some _ ->
      print_string ("Test #" ^ (str (n + 1)) ^ ": ");
      flush stdout;
      let x = Stream.next data in
      let expected = x.output in
      let result = tester x.input in
      if (compare expected result)
      then begin
        print_endline "OK";
        run (n + 1, ok + 1)
      end
      else begin
        print_endline "FAIL";
        print_endline ("   Expected: " ^ (string_of expected));
        print_endline (" Got result: " ^ (string_of result));
        run (n + 1, ok)
      end
  in
  print_endline ("### Testing of <" ^ name ^ ">");
  run (0, 0);;