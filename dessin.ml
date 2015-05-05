(**************************************************************************)
(*                                                                        *)
(*                             ocp-dessin                                 *)
(*                                                                        *)
(*                   Copyright 2015, INRIA/OCamlPro                       *)
(*                                                                        *)
(*  All rights reserved.  Released under the terms of the GPL License     *)
(*                                                                        *)
(**************************************************************************)

(* TODO:
   * afficher un mode interactif, ou lent, permettant d'attendre avant
     [effacer_tortue].
   * utiliser une pile pour [afficher_tortue], permettant de ne l'afficher
     que en mode [interactif] et pas dans les répétitions.
*)
type couleur =
| Noir
| Blanc
| Rouge
| Vert
| Bleu
| Jaune

type command =
| Avancer of int
| Gauche of int
| Droite of int

| Couleur of couleur
| Lever
| Baisser

| Repeter of int * command list

type crayon = couleur -> command

let avancer n = Avancer n
let gauche n = Gauche n
let droite n = Droite n
let crayon couleur = Couleur couleur
let lever crayon = Lever
let baisser crayon = Baisser
let reculer n = Avancer (-n)

let repeter n commands = Repeter (n, commands)

exception TropDeTravail

let width = 1000.
let height = 600.
let graph = ref None
let x = ref (width /. 2.)
let y = ref (height /. 2.)
let direction = ref 0
let couleur = ref Noir
let pression = ref 0 (* > 0 indique qu'on appuie *)
let nsteps = ref 0
let max_steps = ref 100_000_000

let afficher_tortue () = (* XOR ou copie ? *)
  ()

let effacer_tortue () = (* XOR ou copie ? *)
  ()

let  graph () =
  match !graph with
  | Some g -> g
  | None ->
    let g = Graphics.open_graph (Printf.sprintf " %.0fx%.0f" width height) in
    graph := Some g;
    g

let radian_by_degree = atan 1. /. 45.
let angle () = float !direction *. radian_by_degree

let rec dessiner commands =
  match commands with
    [] -> ()
  | cmd :: commands ->
    dessiner_command cmd;
    dessiner commands

and dessiner_command cmd =
  decr nsteps;
  if !nsteps = 0 then raise TropDeTravail;
  match cmd with
  | Avancer n ->
    effacer_tortue ();
    let old_x = !x in
    let old_y = !y in
    x := !x +. float n *. cos (angle ());
    while !x >= width do x := !x -. width done;
    while !x < 0. do x := !x +. width done;
    y := !y +. float n *. sin (angle ());
    while !y >= height do y := !y -. height done;
    while !y < 0. do y := !y +. height done;
    Printf.eprintf "x=%.2f y=%.2f\n%!" (!x) (!y);
    if !pression > 0 then begin
      Graphics.moveto (int_of_float old_x) (int_of_float old_y);
      Graphics.lineto (int_of_float !x) (int_of_float !y);
    end;
    afficher_tortue ();
  | Gauche n ->
    effacer_tortue ();
    direction := !direction + n;
    afficher_tortue ();
  | Droite n ->
    effacer_tortue ();
    direction := !direction - n;
    afficher_tortue ();
  | Lever -> decr pression
  | Baisser -> incr pression
  | Couleur c ->
    Graphics.set_color (match c with
    | Noir -> Graphics.black
    | Blanc -> Graphics.white
    | Rouge -> Graphics.red
    | Vert -> Graphics.green
    | Bleu -> Graphics.blue
    | Jaune -> Graphics.yellow
    );
    couleur := c
  | Repeter (n, commands) ->
    for i = 1 to n do
      dessiner commands
    done

let dessiner commands =
  nsteps := !max_steps;
  dessiner commands

let _ =
  ignore (graph ());
  dessiner [

    (*
      repeter 100 [
      gauche 9;

      crayon Noir;
      baisser crayon;
      avancer 50;
      lever crayon;
      avancer 50;

      crayon Rouge;
      baisser crayon;
      avancer 50;
      lever crayon;

      reculer 150;
      ]
    *)

    (*
    repeter 36 [
      gauche 10;

      repeter 180 [
        baisser crayon;
        gauche 1;
        avancer 2;
        lever crayon;

        gauche 1;
        avancer 2;
      ];

    ];
    *)

    baisser crayon;
    repeter 189 [
      gauche 13;
      repeter 360 [ gauche 1; avancer 2 ];
    ];

  ];
  let rec iter () =
    match Graphics.wait_next_event [ Graphics.Key_pressed ] with
      { Graphics.keypressed = true } ->
        Graphics.close_graph (graph ())
    | _ -> iter ()
  in
  iter ()
