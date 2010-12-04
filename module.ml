open MyUtil

type template =
  | T_Type of Id.t * Id.t list* TypingType.oType
  | T_Function of Id.t * Id.t list * TypingType.oType * Typing.result

type instance =
  | I_Type of Id.t * Type.t list * Type.t
  | I_Function of Id.t * Type.t list * Type.t * Typing.result

type elt =
  | Template of template
  | Instance of instance

type extToIntMap = string Id.Map.t

type intToExtMap = string Id.Map.t

type t = extToIntMap * intToExtMap * elt list

