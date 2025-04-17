module type OrderedTypeExt = sig
  include Map.OrderedType

  val to_string : t -> string
end

module type SExt = sig
  include Map.S

  val to_string : ('a -> string) -> 'a t -> string
end

module MakeExt (Ord : OrderedTypeExt) = struct
  include Map.Make (Ord)

  let to_string to_string m =
    "["
    ^ (m |> to_list
      |> List.map (fun (k, v) -> Ord.to_string k ^ " = " ^ to_string v)
      |> String.concat ", ")
    ^ "]"
end
