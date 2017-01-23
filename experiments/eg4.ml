module type STRINGABLE = sig
  type t
  val to_string: t -> string
end
module type STORE = sig
  module S : STRINGABLE
  type t
  type obj
  val add: obj -> t
  val to_string : t -> string
end

module Store(S: STRINGABLE): STORE with type obj = S.t = struct
  module S = S
  type obj = S.t
  type t = T of obj
  let add x = T x
  let to_string (T x) = S.to_string x
end

module SInt = struct
  type t = int
  let to_string = string_of_int
end

module SBool = struct
  type t = bool
  let to_string = string_of_bool
end

module S1 = Store(SInt)
module S2 = Store(SBool)

let _ = S2.to_string @@ S1.add 2;;
