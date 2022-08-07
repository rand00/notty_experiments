
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

  let sine t_orig (w, h) =
    let t_orig = float t_orig *. 2.0 |> truncate in
    (*< influences speed of sine movement*)
    let t_factor = 0.04 in (*< influences samplings of sine in image*)
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

  let history acc image =
    let open Notty in
    I.(acc </> image)
  
end

let image_e =
  S.sample Image.sine tick_e dimensions_s
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



