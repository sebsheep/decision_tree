open BooleanFormulae
open BooleanFormulae.Parser


let () =
  let quit = ref false in
  while not !quit do
    print_endline "Enter a boolean formula";
    let query = read_line () in
    print_endline query;
    if query == "exit"
    then quit := true
    else  match parse query with
      | Ok formula ->
        print_endline @@ show_tformula formula;
        if isTautology formula then
          print_endline "This formula is a tautology"
        else
          print_endline "This formula is not a tautology";

        let inner_quit = ref false in
        while not !inner_quit do
          print_endline "What do you want to do?";
          print_endline "* enter another formula: type 'o'";
          print_endline "* get the .dot file: type 'd <namefile>.dot'"; 
          match String.trim @@ read_line () with
          | "o" -> inner_quit := true
          | q -> match String.split_on_char ' ' q with
            | ["d"; filename] -> print_endline filename
            | _ -> print_endline "I didn't understand. Try again please!";

        done;

      | Error _ ->
        print_endline "Wrong formula";
  done;
