module D = Domain
module Analysis = Analysis.Make

let usage = "Usage: analyzer [file]"

let main argv =
  if Array.length argv <> 2 then (
    prerr_endline "analyzer: You must specify one LLVM IR file";
    prerr_endline usage;
    exit 1
  );
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file argv.(1) in
  let llm = Llvm_irreader.parse_ir llctx llmem in
  let fenv = Llvm.fold_left_functions (
    fun fenv f ->
      let name = Llvm.value_name f in
      if
        not (
          Utils.is_llvm_function f ||
          name = "malloc" ||
          name = "free"
        )
      then
        let summary = Analysis.analyze_function f fenv in
        D.FunctionEnv.add name summary fenv
      else
        fenv
  ) D.FunctionEnv.empty llm in
  Format.printf "%a" D.FunctionEnv.pp fenv

let _ = main Sys.argv
