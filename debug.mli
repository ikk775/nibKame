val dbglevel : int ref
val dbglevel_stack : int list ref
val setDbglevel : int -> unit
val restoreDbglevel : unit -> unit
val dbg_formatter : Format.formatter ref
val dbgprint : ?level:int -> string -> unit
val dbgprintsexpr : ?level:int -> Sexpr.t -> unit
