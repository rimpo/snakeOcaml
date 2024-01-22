[@@@ocaml.warning "-32"]

open Snake

let width = 50
let height = 50
let cell_size = 10

let setup width height =
  Raylib.init_window
    ((width * cell_size) + (2 * cell_size))
    ((height * cell_size) + (2 * cell_size))
    "snake";
  Raylib.set_target_fps 20;
  make_board width height (make_food width height)
;;

let rec print_snake sn =
  match sn.pos with
  | [] -> print_endline ";"
  | (x, y) :: t ->
    Printf.printf "(%d %d)" x y;
    print_snake { pos = t; d = sn.d; h = sn.h }
;;

let rec loop bo sn =
  if Raylib.window_should_close ()
  then Raylib.close_window ()
  else
    let open Raylib in
    match !sn.h with
    | Dead ->
      begin_drawing ();
      draw_text "DEAD !!" 200 200 20 Color.red;
      draw_text "Press Space to Restart" 120 230 20 Color.red;
      end_drawing ();
      if is_key_down Key.Space
      then loop (make_board width height (make_food width height)) (ref (make_snake ()))
      else loop bo sn
    | Alive ->
      if is_key_down Key.Down && !sn.d != Up
      then sn := { pos = !sn.pos; d = Down; h = !sn.h };
      if is_key_down Key.Up && !sn.d != Down
      then sn := { pos = !sn.pos; d = Up; h = !sn.h };
      if is_key_down Key.Right && !sn.d != Left
      then sn := { pos = !sn.pos; d = Right; h = !sn.h };
      if is_key_down Key.Left && !sn.d != Right
      then sn := { pos = !sn.pos; d = Left; h = !sn.h };
      begin_drawing ();
      clear_background Color.black;
      draw_rectangle_lines 10 10 (width * cell_size) (height * cell_size) Color.gray;
      draw_text
        (string_of_int bo.c)
        ((width * cell_size) + (2 * cell_size) - 40)
        15
        20
        Color.yellow;
      clear_board bo;
      place_snake bo !sn;
      place_food bo;
      for i = 0 to (width * height) - 1 do
        match bo.data.(i) with
        | Empty -> ()
        | Food ->
          let x, y = get_position i width in
          draw_rectangle ((x * cell_size) + 10) ((y * cell_size) + 10) 10 10 Color.red
        | Snake ->
          let x, y = get_position i width in
          draw_rectangle ((x * cell_size) + 10) ((y * cell_size) + 10) 10 10 Color.green
      done;
      end_drawing ();
      let sn = move bo !sn in
      loop bo (ref sn)
;;

let _ =
  Printexc.record_backtrace true;
  try loop (setup width height) (ref (make_snake ())) with
  | _ ->
    let backtrace = Printexc.get_raw_backtrace () in
    Printexc.print_raw_backtrace stdout backtrace;
    exit 1
;;
