module D = Domain
module F = Format

module Memory = D.Memory
module Value = D.Value
module Location = D.Location
module Summary = D.Summary
module FEnv = D.FunctionEnv
module LocMap = D.LocMap

module type S = sig
  val eval : Llvm.llvalue -> Memory.t -> Value.t
  val transfer_instr :
    Llvm.llvalue -> Memory.t -> FEnv.t -> Memory.t
  val transfer_instrs :
    Llvm.llvalue list -> Memory.t -> FEnv.t -> Memory.t list
end

module Make : S = struct
  exception Unsupported

  let eval e mem =
    let kind = Llvm.classify_value e in
    match kind with
    | Llvm.ValueKind.ConstantInt -> (
      match Llvm.int64_of_const e with
      | None -> raise Unsupported
      | Some _ -> Value.bottom
    )
    | Llvm.ValueKind.Instruction _ | Argument ->
        Memory.find (Location.of_variable e) mem
    | Llvm.ValueKind.ConstantPointerNull ->
        Value.of_location Location.null
    | _ -> raise Unsupported

  let transfer_instr instr memory fenv =
    let update v = Memory.weak_update
        (Location.of_variable instr) v memory in
    let arg ind = eval (Llvm.operand instr ind) memory in

    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_free instr -> memory
    | Llvm.Opcode.Br | Ret -> memory
    | Llvm.Opcode.Call when Utils.is_malloc instr ->
        instr
        |> Location.of_allocsite
        |> Value.of_location
        |> update
    | Llvm.Opcode.Alloca ->
        instr
        |> Location.of_alloca
        |> Value.of_location
        |> update
    | Llvm.Opcode.PHI ->
        Llvm.incoming instr
        |> List.map (fun e -> eval (fst e) memory)
        |> List.fold_left Value.join Value.bottom
        |> update
    | Llvm.Opcode.BitCast | PtrToInt | IntToPtr | GetElementPtr ->
        arg 0 |> update
    | Llvm.Opcode.Add | Sub | Mul | SDiv | UDiv ->
        Value.join (arg 0) (arg 1) |> update
    | Llvm.Opcode.ICmp -> update Value.bottom
    | Llvm.Opcode.Load ->
        Value.bottom
        |> Value.fold (
          fun l v -> Value.join v (Memory.find l memory)
        ) (arg 0)
        |> update
    | Llvm.Opcode.Store ->
        let v = arg 0 in
        Value.fold (
          fun l m ->
            if l != Location.Null then
              Memory.weak_update l v m
            else
              m
        ) (arg 1) memory
    | Llvm.Opcode.Call ->
      let callee = Utils.call_callee instr in

      let subst =
        List.combine (Utils.call_params instr) (Utils.call_args instr)
        |> List.fold_left (
            fun m (p, a) ->
              LocMap.add (Location.of_symbol p) (eval a memory) m
        ) LocMap.empty in
      let subst_value v =
        Value.fold (
          fun l v1 -> Value.join v1 (
            match LocMap.find_opt l subst with
            | None -> Value.of_location l
            | Some v2 -> v2
          )
        ) v Value.bottom in

      let Summary.Summary (_, rv, mem) =
        FEnv.find (Llvm.value_name callee) fenv in

      let mem_after_subst = LocMap.map subst_value mem in
      let new_memory =
        memory
        |> Memory.to_loc_map
        |> LocMap.fold (
          fun l v m ->
            match LocMap.find_opt l subst with
            | None -> LocMap.add l v m
            | Some v1 -> Value.fold (fun l1 m1 -> LocMap.add l1 v m1) v1 m
        ) mem_after_subst
        |> Memory.of_loc_map in

      let rv_after_subst = subst_value rv in
      if rv_after_subst = Value.bottom then
        new_memory
      else
        let rloc = Location.of_variable instr in
        Memory.weak_update rloc rv_after_subst new_memory
    | _ -> raise Unsupported

  let transfer_instrs instrs memory fenv =
    List.map (fun i -> transfer_instr i memory fenv) instrs

end
