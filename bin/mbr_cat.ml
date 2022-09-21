let sectors (part, free) =
  let stats = Unix.stat part in
  let free = match free with None -> 0 | Some free -> Int32.to_int free in
  (stats.st_size + 511) / 512 + free

let mbr_cat parts =
  let sizes = List.map sectors parts in
  let partitions =
    List.fold_left
      (fun r size ->
         Result.bind r @@ fun (offset, acc) ->
         Mbr.Partition.make ~ty:0x7F (Int32.of_int offset) (Int32.of_int size)
         |> Result.map (fun partition -> offset + size, partition :: acc))
      (Ok (1, []))
      sizes
    |> Result.map snd
    |> Result.get_ok
  in
  let mbr = Mbr.make partitions |> Result.get_ok in
  let hdr = Cstruct.create 512 in
  Mbr.marshal hdr mbr;
  output_string stdout (Cstruct.to_string hdr);
  let empty_block = String.init 512 (Fun.const '\000') in
  List.iter (fun (f, free) ->
      let ic = open_in f in
      let buf = Bytes.create 512 in
      let rec loop written =
        let len = input ic buf 0 512 in
        if len > 0 then begin
          output_string stdout (Bytes.sub_string buf 0 len);
          loop (written + len)
        end else
          written
      in
      let written = loop 0 in
      if written land 511 <> 0 then
        output_string stdout (String.init (512 - written land 511) (Fun.const '\000'));
      match free with
      | None -> ()
      | Some free ->
        for _ = 1 to Option.get (Int32.unsigned_to_int free) do
          output_string stdout empty_block
        done)
    parts

let jump parts =
  if List.length parts > 4
  then `Help (`Auto, None)
  else `Ok (mbr_cat parts)

open Cmdliner

let part_conv =
  let parse s =
    let (let*) = Result.bind in
    match String.split_on_char ':' s with
    | [ part ] ->
      let* part = Arg.conv_parser Arg.non_dir_file part in
      Ok (part, None)
    | [ part ; "" ; free ] ->
      let* part = Arg.conv_parser Arg.non_dir_file part in
      let* free = Arg.conv_parser Arg.int32 free in
      Ok (part, Some free)
    | _ ->
      Error (`Msg "expected format is PART[::FREE]")
  in
  let print ppf = function
    | part, None ->
      Arg.conv_printer Arg.non_dir_file ppf part
    | part, Some free ->
      Format.fprintf ppf "%a::%a"
        Arg.(conv_printer non_dir_file) part
        Arg.(conv_printer int32) free
  in
  Arg.conv ~docv:"PART[::FREE]" (parse, print)

let parts =
  let doc = Printf.sprintf "Contents of the partition and optional free space. \
                            At most four partitions may be specified." in
  let docv = "PART[::FREE]" in
  Arg.(value & pos_all part_conv [] & info [] ~doc ~docv)

let cmd =
  let doc = "Concatenate files into a MBR partitioned file" in
  let info = Cmd.info "mbr-cat" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(ret (const jump $ parts))

let () = exit (Cmd.eval cmd)
