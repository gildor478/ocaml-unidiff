
open OUnit

module SetDiff = 
  Set.Make
    (struct
       type t = string * int * string
       let compare = compare
     end)

let tests = 
  "UniDiff" >::
  (bracket
     (fun () ->
        open_in "test/test01.diff")
     (fun chn ->
        let t = UniDiff.parse (Stream.of_channel chn) in
        let st = 
          UniDiff.fold `New 
            ~with_context:false 
            ~strip:0
            (fun st fn _ pos line ->
               SetDiff.add (fn, pos, line) st)
            SetDiff.empty t
        in
          assert_bool
            "Contains a random new line."
            (SetDiff.mem 
               ("new-oasis/src/cli/Query.ml", 
                157,
                "                   with e ->")
               st))
     (fun chn ->
        close_in chn))

let _lst : test_result list = 
  run_test_tt_main tests
