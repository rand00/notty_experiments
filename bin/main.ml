
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

  let sine ?(speed_factor=2.0) t_orig (w, h) =
    let t_orig = float t_orig *. speed_factor |> truncate in
    (*< influences speed of sine movement*)
    let t_factor = 300. /. (float w *. float h) in
    (*< influences samplings of sine in image*)
    let open Notty in
    let rec aux acc t_in =
      let t = float t_in *. t_factor in
      let y = sin t in
      let x_scaled, did_wrap =
        let scaled = (t -. float t_orig *. t_factor) *. 10. |> Float.round |> truncate in
        scaled mod w, scaled >= w
      in
      if did_wrap then
        acc
      else
        let y_scaled = (y /. 2. +. 0.5) *. float h |> Float.round |> truncate in
        let image = 
          I.string default_attr "x"
          |> I.hpad x_scaled 0
          |> I.vpad y_scaled 0
        in
        aux I.(acc </> image) (succ t_in)
    in
    aux I.empty t_orig

  let sines t (w, h) =
    let open Notty in
    [
      sine ~speed_factor:8. t (w, h / 1);
      sine ~speed_factor:(-3.) t (w, h / 2);
      sine ~speed_factor:(-7.) t (w, h / 3);
      sine ~speed_factor:3.3 t (w, h / 4);
(*      sine ~speed_factor:3.4 t dimensions;
        sine ~speed_factor:3.8 t dimensions;*)
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



