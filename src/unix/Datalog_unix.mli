
(* this file is part of datalog. See README for the license *)

(** {1 Unix Interpreted Predicates} *)

module TopDown = Datalog_top_down

module type S = sig
  module TD : TopDown.S

  val setup_handlers : TD.DB.t -> unit
end

module Make(TD : TopDown.S) : S with module TD = TD

module Default : S
  with type TD.DB.t = TopDown.Default.DB.t
  and module TD.Const = TopDown.Default.Const
