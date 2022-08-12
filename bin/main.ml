
module Term = Notty_lwt.Term 

open Lwt.Infix
open Lwt_react

let fps = 200.
(*let fps = 10.*)

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

  let default_attr = Notty.A.(fg lightred)

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
  
  let history acc image =
    let open Notty in
    I.(acc </> image)

  let no_idea t (w, h) (acc_image_final, acc_image) =
    let open Notty in
    let sub_image =
      I.string A.(fg red) "o"
    in
    let t_x = (sin (float t) /. 2. +. 0.5) *. 3. |> truncate in
    let t_y = t_x in
    let acc_image =
      if t_x mod 3 = 0 then
        let sub_image = I.hpad 0 1 sub_image in
        I.(sub_image <|> acc_image)
      else
        let sub_image = I.vpad 0 1 sub_image in
        I.(sub_image <-> acc_image)
    in
    let acc_image_final =
      I.pad ~l:(w/2) ~t:(h/2) acc_image
    in
    acc_image_final, acc_image
  
end

let image_e =
  S.sample Image.no_idea tick_e dimensions_s
  |> E.fold (CCFun.flip (@@)) Notty.I.(empty, empty)
  |> E.map fst
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



