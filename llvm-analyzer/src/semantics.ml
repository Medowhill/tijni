module D = Domain
module F = Format

module Memory = D.Memory
module Value = D.Value

module type S = sig
  val eval : Llvm.llvalue -> Memory.t -> Value.t
  val transfer_instr : Llvm.llvalue -> Memory.t -> Memory.t
  val transfer_instrs : Llvm.llvalue list -> Memory.t -> Memory.t list
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
        Memory.find (D.Location.of_variable e) mem
    | Llvm.ValueKind.ConstantPointerNull ->
        Value.of_location D.Location.null
    | _ -> raise Unsupported

  let transfer_instr instr memory =
    let update v = Memory.weak_update
        (D.Location.of_variable instr) v memory in
    let arg ind = eval (Llvm.operand instr ind) memory in

    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_free instr -> memory
    | Llvm.Opcode.Br | Ret -> memory
    | Llvm.Opcode.Call when Utils.is_malloc instr ->
        instr
        |> D.Location.of_allocsite
        |> Value.of_location
        |> update
    | Llvm.Opcode.Alloca ->
        instr
        |> D.Location.of_alloca
        |> Value.of_location
        |> update
    | Llvm.Opcode.PHI ->
        Llvm.incoming instr
        |> List.map (fun e -> eval (fst e) memory)
        |> List.fold_left Value.join Value.bottom
        |> update
    | Llvm.Opcode.BitCast | PtrToInt | IntToPtr ->
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
            if l != D.Location.Null then
              Memory.weak_update l v m
            else
              m
        ) (arg 1) memory
    | Llvm.Opcode.Call ->
      let arity = Llvm.num_operands instr - 1 in
      let callee = Llvm.operand instr arity in
      (* TODO *)
      raise Unsupported
    | _ -> raise Unsupported

  let transfer_instrs instrs memory =
    List.map (fun i -> transfer_instr i memory) instrs

end
