module D = Domain
module F = Format

module type S = sig
  module Memory : D.MEMORY_DOMAIN

  module Value : D.VALUE with type t = Memory.Value.t

  val eval : Llvm.llvalue -> Memory.t -> Value.t

  val filter : Llvm.llvalue -> bool -> Memory.t -> Memory.t

  val transfer_label :
    Llvm.llcontext ->
    D.Label.t ->
    D.CallGraph.t ->
    Memory.t ->
    D.CallGraph.t * (D.Label.t * Memory.t) list

  val transfer :
    Llvm.llcontext ->
    Llvm.llvalue ->
    D.CallGraph.t ->
    Memory.t ->
    D.CallGraph.t * (D.Label.t * Memory.t) list
end

module Make (Memory : D.MEMORY_DOMAIN) : S = struct
  exception Unsupported

  module Memory = Memory
  module Value = Memory.Value

  let eval e mem =
    let kind = Llvm.classify_value e in
    match kind with
    | Llvm.ValueKind.ConstantInt -> (
      match Llvm.int64_of_const e with
      | None -> raise Unsupported
      | Some i ->
          i |> Int64.to_int |> Value.Numerical.of_int |> Value.of_numerical
    )
    | Llvm.ValueKind.Instruction _ | Llvm.ValueKind.Argument
    | Llvm.ValueKind.Function ->
        Memory.find (Domain.Location.of_variable e) mem
    | Llvm.ValueKind.ConstantPointerNull ->
        Domain.Location.null |> Value.LocSet.singleton |> Value.of_locset
    | _ -> raise Unsupported

  let filter cond truth memory = match Llvm.icmp_predicate cond with
    | None -> raise Unsupported
    | Some p ->
        let lhs = Llvm.operand cond 0 in
        let rhs = Llvm.operand cond 1 in
        let lhsl = Domain.Location.of_variable lhs in
        let rhsl = Domain.Location.of_variable rhs in
        let lhsv = eval lhs memory in
        let rhsv = eval rhs memory in
        let pred_for_rhs = function
          | Llvm.Icmp.Eq -> Llvm.Icmp.Eq
          | Llvm.Icmp.Ne -> Llvm.Icmp.Ne
          | Llvm.Icmp.Slt | Llvm.Icmp.Ult -> Llvm.Icmp.Sgt
          | Llvm.Icmp.Sle | Llvm.Icmp.Ule -> Llvm.Icmp.Sge
          | Llvm.Icmp.Sgt | Llvm.Icmp.Ugt -> Llvm.Icmp.Slt
          | Llvm.Icmp.Sge | Llvm.Icmp.Uge -> Llvm.Icmp.Sle in
        let pred_by_truth p = if truth then p else Utils.neg_pred p in
        let is_constant op = match Llvm.classify_value op with
          | Llvm.ValueKind.ConstantInt
          | Llvm.ValueKind.Function
          | Llvm.ValueKind.ConstantPointerNull -> true
          | Llvm.ValueKind.Instruction _
          | Llvm.ValueKind.Argument -> false
          | _ -> raise Unsupported in
        match is_constant lhs, is_constant rhs with
        | false, false ->
            let pl = pred_by_truth p in
            let pr = pred_by_truth (pred_for_rhs p) in
            let fvl = Value.filter pl lhsv rhsv in
            let fvr = Value.filter pr rhsv lhsv in
            if fvl = Value.bottom || fvr = Value.bottom then Memory.bottom
            else memory |> Memory.add lhsl fvl |> Memory.add rhsl fvr
        | false, true ->
            let pl = pred_by_truth p in
            let fvl = Value.filter pl lhsv rhsv in
            if fvl = Value.bottom then Memory.bottom
            else memory |> Memory.add lhsl fvl
        | true, false ->
            let pr = pred_by_truth (pred_for_rhs p) in
            let fvr = Value.filter pr rhsv lhsv in
            if fvr = Value.bottom then Memory.bottom
            else memory |> Memory.add rhsl fvr
        | true, true ->
            let abs_truth = truth
              |> Bool.to_int
              |> Value.Numerical.of_int
              |> Value.of_numerical in
            if Value.order abs_truth (eval cond memory) then memory
            else Memory.bottom

  let transfer _ instr callgraph memory =
    let func = Utils.get_function instr in
    let next = Llvm.instr_succ instr in
    let to_next mem = (callgraph, [ (next, mem) ]) in
    let update v = Memory.strong_update
        (Domain.Location.of_variable instr) v memory in
    let arg ind = eval (Llvm.operand instr ind) memory in
    let apply f =
      f (Value.to_numerical (arg 0)) (Value.to_numerical (arg 1))
      |> Value.of_numerical in
    let lift f x = try Some (f x) with Not_found -> None in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_llvm_intrinsic instr ->
        (callgraph, [ (next, memory) ])
    | Llvm.Opcode.Call
      when Utils.is_print_num instr || Utils.is_print_ptr instr
           || Utils.is_print_mem instr ->
        (callgraph, [ (next, memory) ])
    | Llvm.Opcode.Call when Utils.is_input instr ->
        Value.Numerical.top |> Value.of_numerical |> update |> to_next
    | Llvm.Opcode.Call when Utils.is_malloc instr ->
        instr |> Domain.Location.of_allocsite |> Value.LocSet.singleton
        |> Value.of_locset |> update |> to_next
    | Llvm.Opcode.Alloca ->
        instr |> Domain.Location.of_alloca |> Value.LocSet.singleton
        |> Value.of_locset |> update |> to_next
    | Llvm.Opcode.Ret ->
        let nexts = callgraph
          |> Domain.CallGraph.callsites_of func
          |> Domain.CallGraph.CallSites.elements
          |> List.map (fun i -> (Llvm.instr_succ i, memory)) in
        (callgraph, nexts)
    | Llvm.Opcode.PHI ->
        let nonphi inst =
          lift Utils.get_first_nonphi inst
          |> Option.map (fun next ->
               Llvm.incoming instr
               |> List.map (fun e -> eval (fst e) memory)
               |> List.fold_left Value.join Value.bottom
               |> update
               |> (fun m -> (next, m))
             ) in
        let phi inst =
          lift Utils.get_next_phi inst
          |> Option.map (fun next -> (next, memory)) in
        (callgraph, List.filter_map ((|>) instr) [nonphi; phi])
    | Llvm.Opcode.Add ->
        Value.Numerical.add |> apply |> update |> to_next
    | Llvm.Opcode.Sub ->
        Value.Numerical.sub |> apply |> update |> to_next
    | Llvm.Opcode.Mul ->
        Value.Numerical.mul |> apply |> update |> to_next
    | Llvm.Opcode.SDiv | Llvm.Opcode.UDiv ->
        Value.Numerical.div |> apply |> update |> to_next
    | Llvm.Opcode.Br -> (callgraph,
        match Llvm.get_branch instr with
        | Some (`Conditional (cond, b1, b2)) ->
            let is_recursive = Domain.CallGraph.is_recursive func callgraph in
            let aux b m = if is_recursive then m else filter cond b m in
            [
              (Llvm.instr_begin b1, aux true memory);
              (Llvm.instr_begin b2, aux false memory)
            ]
        | Some (`Unconditional b) ->
            [ (Llvm.instr_begin b, memory) ]
        | _ -> raise Unsupported
    )
    | Llvm.Opcode.ICmp -> (
      match Llvm.icmp_predicate instr with
      | None -> raise Unsupported
      | Some p ->
          let l = arg 0 in
          let r = arg 1 in
          let v1 =
            Value.Numerical.cmp p (Value.to_numerical l) (Value.to_numerical r) in
          let v2 =
            let abs_bool_bot = Value.Numerical.bottom in
            let abs_true = Value.Numerical.of_int 1 in
            let abs_false = Value.Numerical.of_int 0 in
            let abs_bool_top = Value.Numerical.join abs_true abs_false in
            let x = Value.to_locset l in
            let y = Value.to_locset r in
            match p with
            | Llvm.Icmp.Eq ->
                if Value.LocSet.is_empty x || Value.LocSet.is_empty y then
                  abs_bool_bot
                else if Value.LocSet.inter x y |> Value.LocSet.is_empty then
                  abs_false
                else if Value.LocSet.cardinal x = 1 && Value.LocSet.cardinal y = 1 then
                  if Domain.CallGraph.is_recursive func callgraph
                  then abs_bool_top
                  else match Value.LocSet.choose x with
                    | Allocsite _ -> abs_bool_top
                    | _ -> abs_true
                else
                  abs_bool_top
            | Llvm.Icmp.Ne ->
                if Value.LocSet.is_empty x || Value.LocSet.is_empty y then
                  abs_bool_bot
                else if Value.LocSet.inter x y |> Value.LocSet.is_empty then
                  abs_true
                else if Value.LocSet.cardinal x = 1 && Value.LocSet.cardinal y = 1 then
                  if Domain.CallGraph.is_recursive func callgraph
                  then abs_bool_top
                  else match Value.LocSet.choose x with
                    | Allocsite _ -> abs_bool_top
                    | _ -> abs_false
                else
                  abs_bool_top
            | _ -> abs_bool_bot in
          let v3 =
            Value.FunSet.cmp p (Value.to_funset l) (Value.to_funset r) in
          v1
          |> Value.Numerical.join v2
          |> Value.Numerical.join v3
          |> Value.of_numerical
          |> update
          |> to_next
    )
    | Llvm.Opcode.Load ->
        Value.bottom
        |> Value.LocSet.fold (
          fun l v -> Value.join v (Memory.find l memory)
        ) (Value.to_locset (arg 0))
        |> update
        |> to_next
    | Llvm.Opcode.Store ->
        let v = arg 0 in
        let locs = arg 1
          |> Value.to_locset
          |> Value.LocSet.remove Null in
        (
          let c = Value.LocSet.cardinal locs in
          if c = 0 then
            Memory.bottom
          else if c = 1 then
            let l = Value.LocSet.choose locs in
            let update = match l with
              | Alloca (Llvm.Before instr) ->
                  let f = Utils.get_function instr in
                  if Domain.CallGraph.is_recursive f callgraph
                  then Memory.weak_update
                  else Memory.strong_update
              | _ -> Memory.weak_update in
            update l v memory
          else
            Value.LocSet.fold (
              fun l m -> Memory.weak_update l v m
            ) locs memory
        ) |> to_next
    | Llvm.Opcode.Call ->
      let arity = Llvm.num_operands instr - 1 in
      let callee = Value.to_funset (arg arity) in
      let new_callgraph =
        Value.FunSet.fold (
          fun f cg -> cg
            |> Domain.CallGraph.add_callsite instr f
            |> Domain.CallGraph.add_calledge func f
        ) callee callgraph in
      let indices =
        let rec aux i = if i >= arity then [] else i :: aux (i + 1) in
        aux 0 in
      let nexts = callee
        |> Value.FunSet.elements
        |> List.map (fun f ->
            let new_memory = List.fold_left (fun m i ->
              (
                if Domain.CallGraph.is_recursive f callgraph
                then Memory.weak_update
                else Memory.strong_update
              ) (Domain.Location.of_variable (Llvm.param f i)) (arg i) m
            ) memory indices in
            (Utils.entry_point f, new_memory)
        ) in
      (new_callgraph, nexts)
    | _ -> raise Unsupported

  let transfer_label llctx label callgraph memory =
    match label with
    | Llvm.At_end _ -> (callgraph, [])
    | Llvm.Before instr -> transfer llctx instr callgraph memory
end
