(**************************************************************************)
(*                                                                        *)
(*                             ocp-dessin                                 *)
(*                                                                        *)
(*                   Copyright 2015, INRIA/OCamlPro                       *)
(*                                                                        *)
(*  All rights reserved.  Released under the terms of the GPL License     *)
(*                                                                        *)
(**************************************************************************)

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

val avancer : int -> command
val reculer : int -> command
val gauche : int -> command
val droite : int -> command
val crayon : couleur -> command
val lever : crayon -> command
val baisser : crayon -> command

val repeter : int -> command list -> command

val dessiner : command list -> unit
