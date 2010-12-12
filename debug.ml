let dbglevel = ref 0

let dbglevel_stack = ref []

let setDbglevel x = 
  dbglevel_stack := !dbglevel :: !dbglevel_stack;
  dbglevel := x

let restoreDbglevel () =
  dbglevel := List.hd !dbglevel_stack;
  dbglevel_stack := List.tl !dbglevel_stack

let dbg_formatter = ref Format.std_formatter

let dbgprint ?(level = 1) s =
  if !dbglevel > level
  then Format.fprintf !dbg_formatter "%s\n" s

let dbgprintsexpr ?(level = 1) s =
  if !dbglevel > level
  then begin
    Sexpr.write !dbg_formatter s; Format.print_newline ()
  end
