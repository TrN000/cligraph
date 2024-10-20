open Cligraph.Graph
open Cligraph.Utils

(* arg handling *)
let input_file = ref ""
let speclist = [ ("-ye", Arg.Set_string input_file, "input file") ]
let usage = "no, not like this."
let x = linspace 0.0 6.283 30
let y = List.map sin x

let main () =
  Arg.parse speclist print_endline usage;

  let data = (x, y) in

  (* let height, width = get_terminal_size () in *)
  let opts =
    {
      rows = 15;
      cols = 100;
      ptype = Scatter { ch = "+"; color = Cligraph.Colors.Blue };
      ticks = true;
    }
  in

  (* printsqueeze (0, opts.cols) (extrange x) true x; *)
  (* printsqueeze (0, opts.rows) (extrange y) false y; *)
  print_endline (create_graph data opts)

let () = main ()
