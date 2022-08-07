
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

(*
let size_s =
  let aux _ term_opt =
    term_opt |> Option.map Term.size
  in
  S.sample aux tick_e term_s
*)

module Image = struct

  open Gg

  let default_attr = Notty.A.(fg blue ++ bg black)

  let sine x (w, h) =
    let x = float x in
    let y = sin x in
    let x_scaled =
      let scaled = x *. 1.5 |> Float.round |> truncate in
      scaled mod w
    in
    let y_scaled = (y /. 2. +. 0.5) *. float h |> Float.round |> truncate in
    let open Notty in
    I.string default_attr "x"
    |> I.hpad x_scaled 0
    |> I.vpad y_scaled 0

  
end

let output_image (image, term) =
  match term with
  | None -> Lwt.return_unit
  | Some term -> Term.image term image

let _output_e =
  let image_e =
    S.sample Image.sine tick_e dimensions_s
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
  Lwt_main.run @@ loop_ticks ()



