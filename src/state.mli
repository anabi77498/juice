(** Representation of dynamic adventure state.

    This module represents the state of an adventure as it is being played,
    including the adventurer's current room, the rooms that have been visited,
    and functions that cause the state to change. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type t
(** The abstract type of values representing the game state. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal
