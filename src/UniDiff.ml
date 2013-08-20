
(* TODO: see http://www.artima.com/weblogs/viewpost.jsp?thread=164293,
 *  - binary format
 *  - no newline
 *)

type change =
  | Context of int (* pos in old file *) * int (* pos in new file *)
  | Add of int (* pos in new file *)
  | Remove of int (* pos in old file *)
type file =
    {
      old_fn: string;
      old_timestamp: string;
      new_fn: string;
      new_timestamp: string;
      changes: (change * string) list;
    }
type t = 
    file list

let parse strm = 
  let buf = Buffer.create 13 in

  let dflt = 
    {
      old_fn = "";
      old_timestamp = "";
      new_fn = "";
      new_timestamp = "";
      changes = [];
    }
  in

  let rec parse_line () = 
    let finish () = 
      let line = Buffer.contents buf in
        Buffer.clear buf;
        line
    in
    try
      let rec parse_line () =
        match Stream.next strm with 
          | '\n' ->
              finish ()
          | c ->
              Buffer.add_char buf c;
              parse_line ()
      in 
        parse_line ()
    with Stream.Failure ->
      finish ()
  in

  let parse_chompline () = 
    Stream.junk strm;
    parse_line ()
  in

  let finalize_file file acc = 
    {file with changes = List.rev file.changes} :: acc
  in

  let decode_position str = 
    Scanf.sscanf 
      (String.sub str 3 (String.length str - 6)) (* Trim '@@' *)
      "-%d,%d +%d,%d" 
      (fun old_pos _ new_pos _ -> old_pos, new_pos)
  in

  let rec parse_header acc file is_old =
    let str = parse_line () in
    let len = String.length str in
    let len_date = String.length "2012-02-22 22:15:31.000000000 +0000" in
    let date = String.sub str (len - len_date) len_date in
    let fn = String.sub str 4 (len - len_date - 4 - 1) in

    let acc, file =
      if (is_old && file.old_fn <> dflt.old_fn) 
        || file.new_fn <> dflt.new_fn then
        (finalize_file file acc), dflt
      else
        acc, file
    in
    let file = 
      if is_old then
        {file with old_fn = fn; old_timestamp = date}
      else
        {file with new_fn = fn; new_timestamp = date}
    in
      parse_diff acc file min_int min_int 

  and add_line acc file old_pos new_pos chng = 
    parse_diff acc
      {file with changes = (chng, parse_chompline ()) :: file.changes}
      old_pos new_pos

  and parse_diff acc file old_pos new_pos = 
      match Stream.npeek 3 strm with 
        | ['-'; '-'; '-'] ->
            parse_header acc file true

        | ['+'; '+'; '+'] ->
            parse_header acc file false

        | '@' :: '@' :: _ ->
            (* position in the file *)
            let old_pos, new_pos = 
              decode_position (parse_line ())
            in
              parse_diff acc file old_pos new_pos

        | '-' :: _ -> 
            add_line acc file (old_pos + 1) new_pos (Remove old_pos) 

        | '+' :: _ ->
            add_line acc file old_pos (new_pos + 1) (Add new_pos)

        | ' ' :: _ ->
            add_line acc file (old_pos + 1) (new_pos + 1) (Context(old_pos, new_pos))

        | [] -> 
            (* EOF *)
            List.rev (finalize_file file acc)

        | _ ->
            (* Junk *)
            let str = parse_line () in
              if String.length str > 4 && String.sub str 0 4 = "diff" then
                ()
              else
                Printf.eprintf "I: Skipping line %s\n%!" (parse_line ());
              parse_diff acc file old_pos new_pos 

  in
    parse_diff [] dflt min_int min_int 

let fold ?(with_context=true) ?strip fold_type f a t =
  let rec fstrip strip s = 
    try 
      match strip with 
        | None ->
            (* If strip not defined, return basename *)
            let idx = String.rindex s '/' in
              String.sub s (idx + 1) (String.length s - idx - 1)

        | Some 0 ->
            s

        | Some i ->
            let idx = ref (String.index s '/') in
              incr idx;
              while !idx < String.length s && s.[!idx] = '/' do
                incr idx
              done;
              fstrip 
                (Some (i - 1))
                (String.sub s !idx (String.length s - !idx))

    with Not_found ->
      s
  in

    List.fold_left
      (fun a file ->
         let fn, timestamp =
           match fold_type with
             | `Old -> file.old_fn, file.old_timestamp
             | `New -> file.new_fn, file.new_timestamp
         in
         let fn = fstrip strip fn in
           List.fold_left
             (fun a (chng, line) ->
                match chng, fold_type, with_context with
                  | Context (pos, _), `Old, true
                  | Context (_, pos), `New, true
                  | Remove pos, `Old, _ 
                  | Add pos, `New, _ ->
                      f a fn timestamp pos line
                  | _ ->
                      a)
             a file.changes)
      a t

let iter ?with_context ?strip fold_type f t =
  fold 
    ?with_context
    ?strip
    fold_type
    (fun () fn tm pos ln ->
       f fn tm pos ln)
    () t



