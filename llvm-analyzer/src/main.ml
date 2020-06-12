module ValueWithSign = Domain.Value (Domain.Sign)
module SignMemory = Domain.Memory (ValueWithSign)
module SignAnalysis = Analysis.Make (SignMemory)
module ValueWithInterval = Domain.Value (Domain.Interval)
module IntervalMemory = Domain.Memory (ValueWithInterval)
module IntervalAnalysis = Analysis.Make (IntervalMemory)

let usage = "Usage: analyzer [ sign | interval ] [ LLVM IR file ]"

let main argv =
  if Array.length argv <> 3 then (
    prerr_endline "analyzer: You must specify one analysis and one LLVM IR file";
    prerr_endline usage;
    exit 1 );
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file argv.(2) in
  let llm = Llvm_irreader.parse_ir llctx llmem in
  let worklist = Domain.Worklist.init llm in
  match argv.(1) with
  | "sign" ->
      (Domain.CallGraph.empty, SignAnalysis.initialize llm)
      |> SignAnalysis.run llctx Analysis.Widen worklist
      |> snd |> SignAnalysis.check llctx
  | "interval" ->
      (Domain.CallGraph.empty, IntervalAnalysis.initialize llm)
      |> IntervalAnalysis.run llctx Analysis.Widen worklist
      |> IntervalAnalysis.run llctx Analysis.Narrow worklist
      |> snd
      |> IntervalAnalysis.check llctx
  | x ->
      prerr_endline (x ^ " is not a valid analysis");
      prerr_endline usage;
      exit 1

let _ = main Sys.argv
