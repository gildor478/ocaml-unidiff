
open OUnit2

module SetDiff = 
  Set.Make
    (struct
       type t = string * int * string
       let compare = compare
     end)

let extract_diff test_ctxt fn = 
  let chn = open_in (in_testdata_dir test_ctxt [fn]) in
  let t = UniDiff.parse (Stream.of_channel chn) in
    UniDiff.iter `New
      ~with_context:false
      ~strip:0
      (fun fn tm pos ln ->
         logf test_ctxt `Info "'%s', time %s +++ '%s', line %d"
           fn (match tm with Some s -> s | None -> "none")
           ln pos)
      t;
    UniDiff.iter `Old
      ~with_context:false
      ~strip:0
      (fun fn tm pos ln ->
         logf test_ctxt `Info "'%s', time: %s --- '%s', line %d"
           fn (match tm with Some s -> s | None -> "none")
           ln pos)
      t;
    UniDiff.fold `New 
      ~with_context:false 
      ~strip:0
      (fun st fn _ pos line ->
         SetDiff.add (fn, pos, line) st)
      SetDiff.empty t

let tests = 
  "UniDiff" >:::
  [
    "test01.diff" >::
    (fun test_ctxt ->
       let st = extract_diff test_ctxt "test01.diff" in
         assert_bool
           "Contains a random new line."
           (SetDiff.mem 
              ("new-oasis/src/cli/Query.ml", 157,
               "                   with e ->")
              st));

    "test02.diff" >::
    (fun test_ctxt ->
       let st = extract_diff test_ctxt "test02.diff" in
         assert_bool
           "Contains a random new line."
           (SetDiff.mem 
              ("b/test/test.ml", 29, "")
              st));

    "test03.diff" >::
    (fun test_ctxt ->
       let _st: SetDiff.t = extract_diff test_ctxt "test03.diff" in
         ());
  ]

let () = 
  run_test_tt_main tests
