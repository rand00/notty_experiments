
module Term = Notty_lwt.Term 

open Lwt.Infix
open Lwt_react

let fps = 200.

let tick_e, tick_eupd = E.create ()

let term_s, term_supd = S.create None 

let dimensions_s =
  let init = 0, 0 in
  let aux _ term = Option.map Term.size term in
  S.sample aux tick_e term_s
  |> E.map (function
    | None -> init
    | Some v -> v
  )
  |> S.hold init

let sp = Printf.sprintf

module Image = struct

  open Gg

  let default_attr = Notty.A.(fg red)

  let render_f ?(speed=2.0) ?(move=(0,0)) ~f t_orig (w, h) =
    let move_x, move_y = move in
    let t_orig = float t_orig *. speed |> truncate in
    (*< influences speed of sine movement*)
    let t_factor = 0.004 *. log (float w *. float h) in
    (*< influences samplings of sine in image*)
    let open Notty in
    let rec aux acc t_in =
      let t = float t_in *. t_factor in
      let y = f t in
      let x_scaled, did_wrap =
        let scaled = (t -. float t_orig *. t_factor) *. 10. |> Float.round |> truncate in
        scaled mod w, scaled >= w
      in
      if did_wrap then
        acc
      else
        let y_scaled = (y /. 2. +. 0.5) *. float h |> Float.round |> truncate in
        let image = 
          I.string default_attr "~"
          |> I.hpad (move_x + x_scaled) 0
          |> I.vpad (move_y + y_scaled) 0
        in
        aux I.(acc </> image) (succ t_in)
    in
    aux I.empty t_orig

  let sines t (w, h) =
    let open Notty in
    let s0_t_scaled = float t /. 40. in
    let s0_h_f = 1. +. 35. *. (sin s0_t_scaled +. 1.) /. 2. in
    let s0_h = s0_h_f |> truncate in
    let s0_speed = 0.4 *. (sin s0_t_scaled +. 1.) /. 2. in
    let s0_move_y_f = 1. +. 5. *. (sin s0_t_scaled +. 1.) /. 2. in
    let s0_move_y = s0_move_y_f |> truncate in
    let s0_move = 0, s0_move_y in
    let s1_move = 0, s0_h in
    let s1_speed = s0_move_y_f *. 0.2 in
    let s1_t = t * 3 in
    let sin_sum t =
      (
        sin t
        +. sin (s0_move_y_f *. 1.)
        +. sin (t *. 0.3)
        +. sin (s0_h_f *. 0.732)
        +. sin (t +. s0_h_f *. 0.23)
      )
      /. 5.
    in
    [
      render_f ~f:sin ~speed:s0_speed ~move:s0_move t (w, s0_h);
      render_f ~f:sin ~speed:s1_speed ~move:s1_move s1_t (w, h);
      render_f ~f:sin_sum t (w, h);
      render_f ~f:sin ~move:(s0_h * 2, s0_h/2) t (50, 30);
    ]
    |> I.zcat 
  
  let history acc image =
    let open Notty in
    I.(acc </> image)
  
end

let image_e =
  S.sample Image.sines tick_e dimensions_s
(*|> E.fold Image.history Notty.I.empty*)

let _output_e =
  let output_image (image, term) =
    match term with
    | None -> Lwt.return_unit
    | Some term -> Term.image term image
  in
  S.sample (fun v v' -> v, v') image_e term_s
  |> E.map_s output_image

let loop_ticks () =
  let rec aux i =
    Lwt_unix.sleep @@ 1. /. fps >>= fun () -> 
    tick_eupd i;
    aux @@ succ i
  in
  aux 0

let () =
  let term = Term.create ~nosig:false () in
  Lwt_main.at_exit (fun () -> Term.release term);
  term_supd @@ Some term;
  Lwt_main.run Lwt.Infix.(
    loop_ticks () 
  )



