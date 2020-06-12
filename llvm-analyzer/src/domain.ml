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

  val widen : t -> t -> t

  val narrow : t -> t -> t
end

module type NUMERICAL_DOMAIN = sig
  include CPO

  val top : t

  val of_int : int -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val cmp : Llvm.Icmp.t -> t -> t -> t

  val filter : Llvm.Icmp.t -> t -> t -> t
end

module type POWSET_DOMAIN = sig
  module Elt : SET
  module Numerical : NUMERICAL_DOMAIN

  include Set.S with type elt = Elt.t

  include CPO with type t := t

  val cmp : Llvm.Icmp.t -> t -> t -> Numerical.t

  val filter : Llvm.Icmp.t -> t -> t -> t
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
    | Alloca of Label.t
    | Allocsite of Label.t

  let compare = compare

  let null = Null

  let of_variable x = Variable x

  let of_alloca i = Alloca (Label.make i)

  let of_allocsite a = Allocsite (Label.make a)

  let is_null = function Null -> true | _ -> false

  let is_variable = function Variable _ -> true | _ -> false

  let is_alloca = function Alloca _ -> true | _ -> false

  let is_allocsite = function Allocsite _ -> true | _ -> false

  let pp fmt = function
    | Null -> F.fprintf fmt "Null"
    | Variable x -> Utils.string_of_exp x |> F.fprintf fmt "%s"
    | Alloca a -> F.fprintf fmt "Alloca(%a)" Label.pp a
    | Allocsite a -> F.fprintf fmt "Allocsite(%a)" Label.pp a
end

module Function = struct
  type t = Llvm.llvalue

  let compare = compare

  let pp fmt f = F.fprintf fmt "Function(%s)" (Utils.string_of_function f)
end

module type SIGN = sig
  type t = Bot | Pos | Neg | Zero | Top

  include NUMERICAL_DOMAIN with type t := t
end

module Sign : SIGN = struct
  type t = Bot | Pos | Neg | Zero | Top

  let compare = compare

  let bottom = Bot

  let top = Top

  let order x y = match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | _, Top -> true
    | Top, _ -> false
    | x, y -> x = y

  let join x y = match x, y with
    | Top, _ | _, Top -> Top
    | v, Bot | Bot, v -> v
    | x, y when x = y -> x
    | _ -> Top

  let meet x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | v, Top | Top, v -> v
    | x, y when x = y -> x
    | _ -> Bot

  let of_int i =
    if i > 0 then Pos
    else if i < 0 then Neg
    else Zero

  let add v1 v2 = match v1, v2 with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | v, Zero | Zero, v -> v
    | Pos, Pos -> Pos
    | Neg, Neg -> Neg
    | Pos, Neg | Neg, Pos -> Top

  let sub v1 v2 = match v1, v2 with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | v, Zero -> v
    | Pos, Pos | Neg, Neg -> Top
    | _, Neg -> Pos
    | _, Pos -> Neg

  let mul v1 v2 = match v1, v2 with
    | Bot, _ | _, Bot -> Bot
    | Zero, _ | _, Zero -> Zero
    | Top, _ | _, Top -> Top
    | Pos, Pos | Neg, Neg -> Pos
    | Pos, Neg | Neg, Pos -> Neg

  let div v1 v2 = match v1, v2 with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top | _, Zero -> Top
    | Zero, _ -> Zero
    | Pos, Pos | Neg, Neg -> Pos
    | Pos, Neg | Neg, Pos -> Neg

  let cmp pred v1 v2 = match pred with
    | Llvm.Icmp.Eq -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Zero, Zero -> Pos
        | x, y when x = y -> Top
        | _ -> Zero
    )
    | Llvm.Icmp.Ne -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Zero, Zero -> Zero
        | x, y when x = y -> Top
        | _ -> Pos
    )
    | Llvm.Icmp.Ugt | Llvm.Icmp.Sgt -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Pos, Pos | Neg, Neg -> Top
        | Pos, _ -> Pos
        | _, Pos -> Zero
        | _, Zero -> Zero
        | Zero, Neg -> Pos
    )
    | Llvm.Icmp.Uge | Llvm.Icmp.Sge -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Pos, Pos | Neg, Neg -> Top
        | Pos, _ -> Pos
        | _, Pos -> Zero
        | Zero, _ -> Pos
        | Neg, Zero -> Zero
    )
    | Llvm.Icmp.Ult | Llvm.Icmp.Slt -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Pos, Pos | Neg, Neg -> Top
        | Neg, _ -> Pos
        | _, Neg -> Zero
        | _, Zero -> Zero
        | Zero, Pos -> Pos
    )
    | Llvm.Icmp.Ule | Llvm.Icmp.Sle -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | Pos, Pos | Neg, Neg -> Top
        | Neg, _ -> Pos
        | _, Neg -> Zero
        | Zero, _ -> Pos
        | Pos, Zero -> Zero
    )

  let filter pred v1 v2 = match pred with
    | Llvm.Icmp.Eq -> meet v1 v2
    | Llvm.Icmp.Ne -> (
        match v1, v2 with
        | Bot, _ | _, Bot | Zero, Zero -> Bot
        | v, _ -> v
    )
    | Llvm.Icmp.Ugt | Llvm.Icmp.Sgt -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | v, (Top | Neg) -> v
        | v, (Pos | Zero) -> meet v Pos
    )
    | Llvm.Icmp.Uge | Llvm.Icmp.Sge -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | v, (Top | Neg) -> v
        | v, Pos -> meet v Pos
        | v, Zero -> join (meet v Pos) (meet v Zero)
    )
    | Llvm.Icmp.Ult | Llvm.Icmp.Slt -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | v, (Top | Pos) -> v
        | v, (Neg | Zero) -> meet v Neg
    )
    | Llvm.Icmp.Ule | Llvm.Icmp.Sle -> (
        match v1, v2 with
        | Bot, _ | _, Bot -> Bot
        | v, (Top | Pos) -> v
        | v, Neg -> meet v Neg
        | v, Zero -> join (meet v Neg) (meet v Zero)
    )

  let widen = join

  let narrow _ _ = failwith "Unsupported"

  let pp fmt = function
    | Bot -> Format.fprintf fmt "Bot"
    | Pos -> Format.fprintf fmt "Pos"
    | Neg -> Format.fprintf fmt "Neg"
    | Zero -> Format.fprintf fmt "Zero"
    | Top -> Format.fprintf fmt "Top"
end

module type INTERVAL = sig
  type elt = Int of int | PInf | MInf

  type t = Bot | Interval of elt * elt

  include NUMERICAL_DOMAIN with type t := t
end

module Interval : INTERVAL = struct
  type elt = Int of int | PInf | MInf

  let ( <=: ) n m = match n, m with
    | MInf, _ | _, PInf -> true
    | _, MInf | PInf, _ -> false
    | Int m0, Int m1 -> m0 <= m1
  let ( >=: ) n m = m <=: n
  let ( <: ) n m = not (n >=: m)
  let ( >: ) n m = not (n <=: m)

  let elt_max n m = if n <: m then m else n
  let elt_min n m = if n <: m then n else m

  let ( +: ) n m =
    match n, m with
    | MInf, _ | _, MInf -> MInf
    | PInf, _ | _, PInf -> PInf
    | Int n, Int m -> Int (n + m)

  let ( -: ) n m = match n, m with
    | MInf, _ | _, PInf -> MInf
    | PInf, _ | _, MInf -> PInf
    | Int n, Int m -> Int (n - m)

  let ( *: ) n m = match n, m with
    | MInf, PInf | PInf, MInf -> MInf
    | PInf, PInf | MInf, MInf -> PInf
    | MInf, Int n | Int n, MInf ->
      if n < 0 then PInf
      else if n = 0 then Int 0
      else MInf
    | PInf, Int n | Int n, PInf ->
      if n < 0 then MInf
      else if n = 0 then Int 0
      else PInf
    | Int m0, Int m1 -> Int (m0 * m1)

  let ( /: ) n m = match n, m with
    | _, (PInf | MInf) -> Int 0
    | MInf, Int n -> if n < 0 then PInf else MInf
    | PInf, Int n -> if n < 0 then MInf else PInf
    | Int m0, Int m1 -> Int (m0 / m1)

  let elt_map f = function
    | Int n -> Int (f n)
    | v -> v

  type t = Bot | Interval of elt * elt

  let compare = compare

  let bottom = Bot

  let top = Interval (MInf, PInf)

  let of_int i = Interval (Int i, Int i)

  let order x y = match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | Interval (n0, n1), Interval (m0, m1) ->
        m0 <=: n0 && n1 <=: m1

  let add x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval (m0, m1) ->
        Interval (n0 +: m0, n1 +: m1)

  let sub x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval (m0, m1) ->
        Interval (n0 -: m1, n1 -: m0)

  let mul x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval (m0, m1) ->
        let l0 = n0 *: m0 in
        let l1 = n0 *: m1 in
        let l2 = n1 *: m0 in
        let l3 = n1 *: m1 in
        let l = [l1; l2; l3] in
        Interval (List.fold_left elt_min l0 l, List.fold_left elt_max l0 l)

  let div x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval (m0, m1) ->
        if order (of_int 0) y then top
        else
          let l0 = n0 /: m0 in
          let l1 = n0 /: m1 in
          let l2 = n1 /: m0 in
          let l3 = n1 /: m1 in
          let l = [l1; l2; l3] in
          Interval (List.fold_left elt_min l0 l, List.fold_left elt_max l0 l)

  let join x y = match x, y with
    | Bot, v | v, Bot -> v
    | Interval (n0, n1), Interval (m0, m1) ->
        Interval (elt_min n0 m0, elt_max n1 m1)

  let meet x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval(m0, m1) ->
        let lb = elt_max n0 m0 in
        let ub = elt_min n1 m1 in
        if lb <=: ub then Interval (lb, ub) else Bot

  let widen x y = match x, y with
    | Bot, v | v, Bot -> v
    | Interval (n0, n1), Interval (m0, m1) ->
        Interval (
          (if m0 <: n0 then MInf else n0),
          (if m1 >: n1 then PInf else n1)
        )

  let narrow x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Interval (n0, n1), Interval (m0, m1) ->
        Interval (
          (if n0 = MInf then m0 else n0),
          (if n1 = PInf then m1 else n1)
        )

  let pp_elt fmt = function
    | Int i -> Format.fprintf fmt "%d" i
    | PInf -> Format.fprintf fmt "+oo"
    | MInf -> Format.fprintf fmt "-oo"

  let pp fmt = function
    | Bot -> Format.fprintf fmt "Bot"
    | Interval (l, u) -> Format.fprintf fmt "[%a, %a]" pp_elt l pp_elt u

  let cmp pred x y =
    let abs_true = of_int 1 in
    let abs_false = of_int 0 in
    let abs_bool_top = join abs_true abs_false in
    match pred with
    | Llvm.Icmp.Eq -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n0 = n1 && n1 = m0 && m0 = m1 then abs_true
            else if n1 <: m0 || m1 <: n0 then abs_false
            else abs_bool_top
    )
    | Llvm.Icmp.Ne -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n0 = n1 && n1 = m0 && m0 = m1 then abs_false
            else if n1 <: m0 || m1 <: n0 then abs_true
            else abs_bool_top
    )
    | Llvm.Icmp.Ugt | Llvm.Icmp.Sgt -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n0 >: m1 then abs_true
            else if n1 <=: m0 then abs_false
            else abs_bool_top
    )
    | Llvm.Icmp.Uge | Llvm.Icmp.Sge -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n0 >=: m1 then abs_true
            else if n1 <: m0 then abs_false
            else abs_bool_top
    )
    | Llvm.Icmp.Ult | Llvm.Icmp.Slt -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n1 <: m0 then abs_true
            else if n0 >=: m1 then abs_false
            else abs_bool_top
    )
    | Llvm.Icmp.Ule | Llvm.Icmp.Sle -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Interval (n0, n1), Interval(m0, m1) ->
            if n1 <=: m0 then abs_true
            else if n0 >: m1 then abs_false
            else abs_bool_top
    )

  let filter pred x y = match pred with
    | Llvm.Icmp.Eq -> meet x y
    | Llvm.Icmp.Ne -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | x, Interval(n, m) ->
            if n = m
            then
              join
                (meet x (Interval (elt_map ((+) 1) n, PInf)))
                (meet x (Interval (MInf, elt_map ((+) (-1)) n)))
            else x
    )
    | Llvm.Icmp.Ugt | Llvm.Icmp.Sgt -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | x, Interval(n, _) -> meet x (Interval (elt_map ((+) 1) n, PInf))
    )
    | Llvm.Icmp.Uge | Llvm.Icmp.Sge -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | x, Interval(n, _) -> meet x (Interval (n, PInf))
    )
    | Llvm.Icmp.Ult | Llvm.Icmp.Slt -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | x, Interval(_, n) -> meet x (Interval (MInf, elt_map ((+) (-1)) n))
    )
    | Llvm.Icmp.Ule | Llvm.Icmp.Sle -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | x, Interval(_, n) -> meet x (Interval (MInf, n))
    )
end

module PowSet (Elt : SET) (Numerical : NUMERICAL_DOMAIN) : POWSET_DOMAIN
        with type Elt.t = Elt.t with type Numerical.t = Numerical.t = struct
  module Elt = Elt
  module Numerical = Numerical
  include Set.Make (Elt)

  let bottom = empty

  let order = subset

  let join = union

  let widen = join

  let narrow _ _ = failwith "Unsupported"

  let cmp pred x y =
    let abs_bool_bot = Numerical.bottom in
    let abs_true = Numerical.of_int 1 in
    let abs_false = Numerical.of_int 0 in
    let abs_bool_top = Numerical.join abs_true abs_false in
    match pred with
    | Llvm.Icmp.Eq ->
        if is_empty x || is_empty y then
          abs_bool_bot
        else if inter x y |> is_empty then
          abs_false
        else if cardinal x = 1 && cardinal y = 1 then
          abs_true
        else
          abs_bool_top
    | Llvm.Icmp.Ne ->
        if is_empty x || is_empty y then
          abs_bool_bot
        else if inter x y |> is_empty then
          abs_true
        else if cardinal x = 1 && cardinal y = 1 then
          abs_false
        else
          abs_bool_top
    | _ -> abs_bool_bot

  let filter pred x y = match pred with
    | Llvm.Icmp.Eq -> inter x y
    | Llvm.Icmp.Ne ->
        let c = cardinal y in
        if c = 0 then bottom else if c = 1 then diff x y else x
    | _ -> bottom

  let pp fmt s =
    F.fprintf fmt "{";
    iter (fun e -> F.fprintf fmt "%a, " Elt.pp e) s;
    F.fprintf fmt "}"
end


module type VALUE = sig
  include CPO

  module Numerical : NUMERICAL_DOMAIN
  module LocSet : POWSET_DOMAIN
    with type Elt.t = Location.t with type Numerical.t = Numerical.t
  module FunSet : POWSET_DOMAIN
    with type Elt.t = Function.t with type Numerical.t = Numerical.t

  val make : Numerical.t * LocSet.t * FunSet.t -> t

  val of_numerical : Numerical.t -> t

  val of_locset : LocSet.t -> t

  val of_funset : FunSet.t -> t

  val to_numerical : t -> Numerical.t

  val to_locset : t -> LocSet.t

  val to_funset : t -> FunSet.t

  val filter : Llvm.Icmp.t -> t -> t -> t
end

module Value (Numerical : NUMERICAL_DOMAIN) : VALUE = struct
  module Numerical = Numerical
  module LocSet = PowSet (Location) (Numerical)
  module FunSet = PowSet (Function) (Numerical)

  type t = Numerical.t * LocSet.t * FunSet.t

  let bottom = (Numerical.bottom, LocSet.bottom, FunSet.bottom)

  let make (i, l, f) = (i, l, f)

  let of_numerical i = (i, LocSet.bottom, FunSet.bottom)

  let of_locset l = (Numerical.bottom, l, FunSet.bottom)

  let of_funset f = (Numerical.bottom, LocSet.bottom, f)

  let to_numerical (i, _, _) = i

  let to_locset (_, l, _) = l

  let to_funset (_, _, f) = f

  let compare = compare

  let order (i1, l1, f1) (i2, l2, f2) =
    Numerical.order i1 i2 && LocSet.order l1 l2 && FunSet.order f1 f2

  let join (i1, l1, f1) (i2, l2, f2) =
    (Numerical.join i1 i2, LocSet.join l1 l2, FunSet.join f1 f2)

  let widen (i1, l1, f1) (i2, l2, f2) =
    (Numerical.widen i1 i2, LocSet.widen l1 l2, FunSet.widen f1 f2)

  let narrow (i1, l1, f1) (i2, _, _) =
    (Numerical.narrow i1 i2, l1, f1)

  let filter cmp (i1, l1, f1) (i2, l2, f2) =
    let l = match cmp with
    | Llvm.Icmp.Eq -> LocSet.inter l1 l2
    | Llvm.Icmp.Ne ->
        let c = LocSet.cardinal l2 in
        if c = 0 then LocSet.bottom
        else if c = 1 then match LocSet.choose l2 with
          | Allocsite _ -> l1
          | _ -> LocSet.diff l1 l2
        else l1
    | _ -> LocSet.bottom in
    (Numerical.filter cmp i1 i2, l, FunSet.filter cmp f1 f2)

  let pp fmt (i, l, f) =
    F.fprintf fmt "(%a, %a, %a)" Numerical.pp i LocSet.pp l FunSet.pp f
end

module type MEMORY_DOMAIN = sig
  include CPO

  module Value : VALUE

  val add : Location.t -> Value.t -> t -> t

  val strong_update : Location.t -> Value.t -> t -> t

  val weak_update : Location.t -> Value.t -> t -> t

  val find : Location.t -> t -> Value.t
end

module Memory (Value : VALUE) : MEMORY_DOMAIN with type Value.t = Value.t =
struct
  module M = Map.Make (Location)
  module Value = Value

  type t = Value.t M.t

  let bottom = M.empty

  let add = M.add

  let compare = compare

  let find x m = try M.find x m with Not_found -> Value.bottom

  let order m1 m2 = M.for_all (fun x v -> Value.order v (find x m2)) m1

  let variable_wise op m1 m2 =
    let default = Value.bottom in
    let m = M.merge (fun _ v1 v2 ->
      Some (op (Option.value v1 ~default) (Option.value v2 ~default))
    ) m1 m2 in
    if M.exists (fun _ v -> v = Value.bottom) m
    then bottom
    else m

  let join = variable_wise Value.join

  let widen = variable_wise Value.widen

  let narrow = variable_wise Value.narrow

  let strong_update = add

  let weak_update k v m = add k (Value.join (find k m) v) m

  let pp fmt m =
    M.iter (fun k v -> F.fprintf fmt "%a -> %a\n" Location.pp k Value.pp v) m
end

module Table (M : MEMORY_DOMAIN) = struct
  include Map.Make (Label)

  let find label tbl = try find label tbl with Not_found -> M.bottom
end

module Worklist = struct
  include Set.Make (Label)

  let rec collect_labels acc pos =
    let acc = add pos acc in
    match pos with
    | Llvm.At_end _ -> acc
    | Llvm.Before instr -> collect_labels acc (Llvm.instr_succ instr)

  let init m =
    Llvm.fold_left_functions
      (fun a func ->
        if Llvm.is_declaration func then a
        else
          Llvm.fold_left_blocks
            (fun a blk ->
              let pos = Llvm.instr_begin blk in
              collect_labels a pos)
            a func)
      empty m
end

module CallGraph = struct
  module CallSites = struct
    include Set.Make (struct
      type t = Llvm.llvalue (* instruction *)

      let compare = compare
    end)

    let pp fmt cs =
      F.fprintf fmt "{";
      iter (fun s -> F.fprintf fmt "%s, " (Utils.string_of_instr s)) cs;
      F.fprintf fmt "}"
  end

  module CallSiteMap = Map.Make (Function)

  module CallEdges = struct
    include Set.Make (struct
      (* pair of functions: caller * callee *)
      type t = Llvm.llvalue * Llvm.llvalue

      let compare = compare
    end)

    let pp fmt s =
      F.fprintf fmt "{";
      iter
        (fun (src, dst) ->
          F.fprintf fmt "(%s, %s)"
            (Utils.string_of_function src)
            (Utils.string_of_function dst))
        s;
      F.fprintf fmt "}"
  end

  type t = { callsites : CallSites.t CallSiteMap.t; calledges : CallEdges.t }

  let empty = { callsites = CallSiteMap.empty; calledges = CallEdges.empty }

  let callsites_of f callgraph =
    try CallSiteMap.find f callgraph.callsites
    with Not_found -> CallSites.empty

  let add_callsite instr f callgraph =
    let callsites =
      CallSiteMap.update f
        (function
          | None -> Some (CallSites.singleton instr)
          | Some s -> Some (CallSites.add instr s))
        callgraph.callsites
    in
    { callgraph with callsites }

  let add_calledge caller callee callgraph =
    {
      callgraph with
      calledges = CallEdges.add (caller, callee) callgraph.calledges;
    }

  module FunSet = CallSites
  let is_recursive f callgraph =
    let rec fixpoint f x =
      let fx = f x in
      if FunSet.equal fx x then fx else fixpoint f fx in
    let find_callees f =
      let es = CallEdges.filter (
        fun (caller, _) -> caller = f
      ) callgraph.calledges in
      CallEdges.fold (
        fun (_, callee) s -> FunSet.add callee s
      ) es FunSet.empty in
    let all_callees = fixpoint (fun fs ->
      let nfs = FunSet.fold (
        fun f nfs -> FunSet.union nfs (find_callees f)
      ) fs FunSet.empty in
      FunSet.union nfs fs
    ) (find_callees f) in
    FunSet.mem f all_callees

  let pp fmt m =
    F.fprintf fmt "=== Call Graph ===\n";
    F.fprintf fmt "- Call Sites\n";
    CallSiteMap.iter
      (fun f callsites ->
        F.fprintf fmt "%a -> %a" Function.pp f CallSites.pp callsites)
      m.callsites;
    F.fprintf fmt "- Call Edges\n";
    F.fprintf fmt "%a\n" CallEdges.pp m.calledges
end
