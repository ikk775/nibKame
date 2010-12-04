type template =
  | T_Type of Id.t * Id.t list* TypingType.typeScheme
  | T_Function of Id.t * Id.t list * Typing.result

type instance =
  | I_Type of Id.t * Type.t list * Type.t
  | I_Function of Id.t * Type.t list * Typing.result

type elt =
  | Template of template
  | Instance of instance

type t = elt list

