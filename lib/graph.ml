type scatter = { ch : string; color : Colors.color }
(** Char to print. should not be something that renders out to longer than 1
    character.
    Color to use for scatter points. will be enclosed in ansi color code and
    reset code. *)

type bar = { color : Colors.color }

type plottype = Scatter of scatter | Line | Bar of bar
type graphoptions = { rows : int; cols : int; ptype : plottype; ticks : bool }

(** Tuple of min and max of a list. *)
let range x = (Utils.foldll1 min x, Utils.foldll1 max x)

let extrange x p =
  let minx, maxx = range x in
  let scale = maxx -. minx in
  (minx -. (p *. scale), maxx +. (p *. scale))

(** Squeeze floating point range into fixed number of lines.
    Returns function for convenience in maps. *)
let discsqueeze lines range rightsideup =
  let linesstart, linesend = lines in
  let linesef = Int.to_float (linesend - 1) in
  let linessf = Int.to_float linesstart in
  let linescale = linesef -. linessf in
  let a, b = range in
  let scale = b -. a in
  if not rightsideup then fun x ->
    Float.to_int
      (Float.round (linesef +. linessf -. ((x -. a) /. scale *. linescale)))
  else fun x ->
    Float.to_int (Float.round (linessf +. ((x -. a) /. scale *. linescale)))

(** Debug print function for discsqueeze. *)
let printsqueeze lines range rsu x =
  let xl = List.map (discsqueeze lines range rsu) x in
  let linst, linen = lines in
  print_int linst;
  print_int linen;
  print_string "\n";
  List.iter
    (fun i ->
      print_int i;
      print_string ", ")
    xl;
  print_endline "";
  () [@warning "-32"]

(** Scatter plot data set into graph. See `scatter` type for options. *)
let scatter x y into graph opts =
  let hl, hr, vt, vb = into in
  let xl = List.map (discsqueeze (hl + 1, hr) (extrange x 0.05) true) x in
  let yl = List.map (discsqueeze (vt, vb) (extrange y 0.05) false) y in
  let { ch; color } = opts in
  Utils.zipwith (fun xi yi -> graph.(yi).(xi) <- Colors.enclose ch color) xl yl

let bar x y into graph opts =
  let hl, hr, vt, vb = into in
  let xl = List.map (discsqueeze (hl + 1, hr) (extrange x 0.05) true) x in
  let yl = List.map (discsqueeze (vt, vb) (extrange y 0.05) false) y in
  let { color } = opts in
  Utils.zipwith
    (fun xi yi -> 
      for i = yi to vb do
        graph.(i).(xi) <- (Colors.enclose Blockchars.full_block color)
      done)
    xl yl


let axesinto opts =
  (* ought to be decided earlier, along with what axis labels are. *)
  let tichset = if opts.ticks then 5 else 0 in
  let ticvset = if opts.ticks then 2 else 0 in
  (tichset, opts.cols, 0, opts.rows - ticvset)

(** Draw string on graph starting at position `pos`. *)
let graphstring pos s graph =
  let x, y = pos in
  String.iteri (fun i c -> graph.(y).(x + i) <- String.make 1 c) s

(** Draw string on graph right justified at position `pos`. *)
let graphstringrj pos s graph =
  let adjpos = (fst pos - String.length s, snd pos) in
  graphstring adjpos s graph

(** Draw string on graph centered on position `pos`. *)
let graphstringc pos s graph =
  let adjpos = (fst pos - (String.length s / 2), snd pos) in
  graphstring adjpos s graph

(** Render graph axes on `graph` using border variables in `into`. *)
let graphaxes xlab ylab into grph =
  let hl, hr, vt, vb = into in
  for i = vt to vb do
    grph.(i).(hl) <- Boxchars.light_vertical
  done;
  for i = hl to hr - 1 do
    grph.(vb).(i) <- Boxchars.light_horizontal
  done;
  grph.(vb).(hl) <- Boxchars.light_up_and_right;
  let xlabtic =
    List.map (discsqueeze (hl, hr) (extrange (fst xlab) 0.05) true) (fst xlab)
  in
  List.iter
    (fun tic -> grph.(vb).(tic) <- Boxchars.light_down_and_horizontal)
    xlabtic;
  Utils.zipwith
    (fun tic value -> graphstringc (tic, vb + 1) value grph)
    xlabtic (snd xlab);
  let ylabtic =
    List.map (discsqueeze (vt, vb) (extrange (fst ylab) 0.05) true) (fst ylab)
  in
  List.iter
    (fun tic -> grph.(tic).(hl) <- Boxchars.light_vertical_and_left)
    ylabtic;
  Utils.zipwith
    (fun tic value -> graphstringrj (hl - 1, tic) value grph)
    ylabtic (snd ylab)

(** Main function of this Library. Creates the string containing the full graph.
    Lines delimited by `\\r\\n`.
    See `graphoptions` for configurable parameters. *)
let create_graph data opts =
  let grph = Array.make_matrix opts.rows opts.cols " " in
  let xs, ys = data in

  let into = axesinto opts in

  let xlab = ([ 0.0; 3.142; 6.283 ], [ "0"; "pi"; "tau" ]) in
  let ylab = ([ 1.0; 0.0; -1.0 ], [ "1"; "0"; "-1" ]) in
  if opts.ticks then graphaxes xlab ylab into grph;

  (match opts.ptype with
  | Scatter sopts -> scatter xs ys into grph sopts
  | Line -> ()
  | Bar bopts -> bar xs ys into grph bopts);

  Utils.intersperse "\n\r" (Array.map (Utils.intersperse "") grph)
