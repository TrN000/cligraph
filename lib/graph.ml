type scatter = { ch : string; color : Colors.color }
type plottype = Scatter of scatter | Line
type graphoptions = { rows : int; cols : int; ptype : plottype; ticks : bool }

let range x = (Utils.foldll1 min x, Utils.foldll1 max x)

let extrange x =
  let minx, maxx = range x in
  let scale = maxx -. minx in
  (minx -. (0.05 *. scale), maxx +. (0.05 *. scale))

(* let extrange x p = *)
(*   let minx, maxx = range x in *)
(*   let scale = maxx -. minx in *)
(*   minx -. p*.scale, maxx +. p*.scale *)

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

let scatter x y into graph opts =
  let hl, hr, vt, vb = into in
  let xl = List.map (discsqueeze (hl + 1, hr) (extrange x) true) x in
  let yl = List.map (discsqueeze (vt, vb) (extrange y) false) y in
  let { ch; color } = opts in
  Utils.zipwith (fun xi yi -> graph.(yi).(xi) <- Colors.enclose ch color) xl yl

let axesinto opts =
  let tichset = if opts.ticks then 5 else 0 in
  let ticvset = if opts.ticks then 2 else 0 in
  (tichset, opts.cols, 0, opts.rows - ticvset)

let graphstring pos s graph =
  let x, y = pos in
  String.iteri (fun i c -> graph.(y).(x + i) <- String.make 1 c) s

let graphstringrj pos s graph =
  let adjpos = (fst pos - String.length s, snd pos) in
  graphstring adjpos s graph

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
    List.map (discsqueeze (hl, hr) (extrange (fst xlab)) true) (fst xlab)
  in
  List.iter
    (fun tic -> grph.(vb).(tic) <- Boxchars.light_down_and_horizontal)
    xlabtic;
  Utils.zipwith
    (fun tic value -> graphstring (tic, vb + 1) value grph)
    xlabtic (snd xlab);
  let ylabtic =
    List.map (discsqueeze (vt, vb) (extrange (fst ylab)) true) (fst ylab)
  in
  List.iter
    (fun tic -> grph.(tic).(hl) <- Boxchars.light_vertical_and_left)
    ylabtic;
  Utils.zipwith
    (fun tic value -> graphstringrj (hl - 1, tic) value grph)
    ylabtic (snd ylab)

let create_graph data opts =
  let grph = Array.make_matrix opts.rows opts.cols " " in
  let xs, ys = data in

  let into = axesinto opts in

  let xlab = ([ 0.0; 3.142; 6.283 ], [ "0"; "pi"; "tau" ]) in
  let ylab = ([ 1.0; 0.0; -1.0 ], [ "1"; "0"; "-1" ]) in
  if opts.ticks then graphaxes xlab ylab into grph;

  (match opts.ptype with
  | Scatter sopts -> scatter xs ys into grph sopts
  | Line -> ());

  Utils.intersperse "\n\r" (Array.map (Utils.intersperse "") grph)
