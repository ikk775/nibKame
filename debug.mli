val dbglevel : int ref
val dbglevel_stack : int list ref
val set_dbglevel : int -> unit
val restore_dbglevel : unit -> unit
val dbg_formatter : Format.formatter ref
val dbgprint : ?level:int -> string -> unit
val dbgprintsexpr : ?level:int -> Sexpr.t -> unit
