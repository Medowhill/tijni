module F = Format

module type SET = sig
  type t

  val compare : t -> t -> int
  val pp : F.formatter -> t -> unit
end

module type CPO = sig
  include SET

  val bottom : t
  val order : t -> t -> bool
  val join : t -> t -> t
end

module type POWSET_DOMAIN = sig
  module Elt : SET
  include Set.S with type elt = Elt.t
  include CPO with type t := t
end

module Label = struct
  type t = (Llvm.llbasicblock, Llvm.llvalue) Llvm.llpos

  let make instr = Llvm.Before instr

  let compare = compare

  let pp fmt = function
    | Llvm.Before instr -> Utils.string_of_instr instr |> F.fprintf fmt "%s"
    | Llvm.At_end _ -> F.fprintf fmt "At_end"
end

module Location = struct
  type t =
    | Null
    | Variable of Llvm.llvalue
    | Symbol of Llvm.llvalue
    | Alloca of Label.t
    | Allocsite of Label.t

  let compare = compare

  let null = Null
  let of_variable x = Variable x
  let of_symbol x = Symbol x
  let of_alloca a = Alloca (Label.make a)
  let of_allocsite a = Allocsite (Label.make a)

  let is_null = function Null -> true | _ -> false
  let is_variable = function Variable _ -> true | _ -> false
  let is_symbol = function Symbol _ -> true | _ -> false
  let is_alloca = function Alloca _ -> true | _ -> false
  let is_allocsite = function Allocsite _ -> true | _ -> false

  let pp fmt = function
    | Null -> F.fprintf fmt "Null"
    | Variable x -> (
        match Llvm.classify_value x with
        | Llvm.ValueKind.Argument ->
            let param = Utils.string_of_exp x in
            let func = x |> Llvm.param_parent |> Llvm.value_name in
            F.fprintf fmt "%s(%s)" param func
        | _ ->
            let var = Utils.string_of_exp x in
            let func = x
              |> Llvm.instr_parent |> Llvm.block_parent |> Llvm.value_name in
            F.fprintf fmt "%s(%s)" var func
    )
    | Symbol x ->
        let param = Utils.string_of_exp x in
        let func = x |> Llvm.param_parent |> Llvm.value_name in
        F.fprintf fmt "\'%s(%s)" param func
    | Alloca a -> F.fprintf fmt "Alloca(%a)" Label.pp a
    | Allocsite a -> F.fprintf fmt "Allocsite(%a)" Label.pp a
end

module type VALUE = sig
  include CPO

  val of_location : Location.t -> t
  val fold : (Location.t -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (Location.t -> bool) -> t -> t
end

module PowSet (Elt : SET) : POWSET_DOMAIN with type Elt.t = Elt.t = struct
  module Elt = Elt
  include Set.Make (Elt)

  let compare = compare

  let bottom = empty

  let order = subset

  let join = union

  let pp fmt s =
    F.fprintf fmt "{";
    iter (fun e -> F.fprintf fmt "%a, " Elt.pp e) s;
    F.fprintf fmt "}"
end

module Value : VALUE = struct
  include PowSet (Location)

  let compare = compare
  let of_location = singleton
  let fold = fold
  let filter = filter
end

module LocMap = Map.Make (Location)

module type MEMORY_DOMAIN = sig
  include CPO

  val add : Location.t -> Value.t -> t -> t
  val weak_update : Location.t -> Value.t -> t -> t
  val find : Location.t -> t -> Value.t

  val to_loc_map : t -> Value.t LocMap.t
  val of_loc_map : Value.t LocMap.t -> t
end

module Memory : MEMORY_DOMAIN = struct
  module M = LocMap

  type t = Value.t M.t

  let bottom = M.empty

  let compare = compare

  let add = M.add

  let find x m = try M.find x m with Not_found -> Value.bottom

  let order m1 m2 = M.for_all (fun x v -> Value.order v (find x m2)) m1

  let join =
    let default = Value.bottom in
    M.merge (
      fun _ v1 v2 ->
        Some (Value.join (Option.value v1 ~default) (Option.value v2 ~default))
    )

  let weak_update k v m = add k (Value.join (find k m) v) m

  let to_loc_map m = m
  let of_loc_map m = m

  let pp fmt m =
    M.iter (fun k v -> F.fprintf fmt "%a -> %a\n" Location.pp k Value.pp v) m
end

module Summary = struct
  type t = Summary of (bool * bool) list * Value.t * Value.t LocMap.t

  let make ps rv mem = Summary (ps, rv, Memory.to_loc_map mem)

  let show = function
    | true, true -> "used & freed"
    | true, false -> "used"
    | false, true -> "freed"
    | false, false -> "-"

  let mem_pp fmt m =
    LocMap.iter (fun k v -> F.fprintf fmt "%a -> %a\n" Location.pp k Value.pp v) m

  let pp fmt = function
    | Summary (ps, rv, mem) ->
      List.iteri (fun i p -> F.fprintf fmt "%d -> %s\n" i (show p)) ps;
      F.fprintf fmt "%a\n" Value.pp rv;
      F.fprintf fmt "%a" mem_pp mem
end

module FunctionEnv = struct
  module M = Map.Make (String)

  type t = Summary.t M.t

  let compare = compare

  let empty = M.empty

  let add = M.add

  let find = M.find

  let pp fmt m =
    M.iter (fun k v -> F.fprintf fmt "[%s]\n%a\n" k Summary.pp v) m
end
