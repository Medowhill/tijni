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
    | Variable x -> Utils.string_of_exp x |> F.fprintf fmt "%s"
    | Symbol x -> Utils.string_of_exp x |> F.fprintf fmt "\'%s"
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

module type MEMORY_DOMAIN = sig
  include CPO

  val add : Location.t -> Value.t -> t -> t
  val weak_update : Location.t -> Value.t -> t -> t
  val find : Location.t -> t -> Value.t
end

module Memory : MEMORY_DOMAIN = struct
  module M = Map.Make (Location)

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

  let pp fmt m =
    M.iter (fun k v -> F.fprintf fmt "%a -> %a\n" Location.pp k Value.pp v) m
end

module Summary = struct
  exception Not_found
  exception Invalid

  type rv =
    | Null
    | Param of int
    | Alloc of Label.t

  type t = Summary of (bool * bool) list * rv list

  let make ps rv = Summary (ps, rv)

  let make_rv v ps =
    let rec index_of p l = match l with
      | [] -> raise Not_found
      | h :: t ->
          if h = p then
            0
          else
            1 + index_of p t in
    Value.fold (
      fun v lst -> (match v with
        | Location.Null -> Null :: lst
        | Location.Symbol p -> Param (index_of p ps) :: lst
        | Location.Allocsite l -> Alloc l :: lst
        | _ -> lst
      )
    ) v []

  let show = function
    | true, true -> "used & freed"
    | true, false -> "used"
    | false, true -> "freed"
    | false, false -> "-"

  let rv_pp fmt = function
    | Null -> F.fprintf fmt "Null"
    | Param i -> F.fprintf fmt "%%%d" i
    | Alloc l -> F.fprintf fmt "%a" Label.pp l

  let pp fmt = function
    | Summary (ps, rv) ->
      List.iteri (fun i p -> F.fprintf fmt "%d -> %s\n" i (show p)) ps;
      F.fprintf fmt "{ ";
      List.iter (F.fprintf fmt "%a, " rv_pp) rv;
      F.fprintf fmt " }\n";
end
