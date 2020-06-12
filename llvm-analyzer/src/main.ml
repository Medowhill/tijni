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
  Llvm.fold_left_functions (
    fun _ f ->
      if not (
        Utils.is_llvm_function f ||
        Llvm.value_name f = "malloc" ||
        Llvm.value_name f = "free"
      ) then (
        let s = Analysis.analyze_function f in
        Format.printf "%a\n" Domain.Summary.pp s
      )
  ) () llm

let _ = main Sys.argv
