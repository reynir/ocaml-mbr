let sectors (part, size_spec) =
  let stats = Unix.stat part in
  (* XXX: warn on misaligned data? *)
  let data_sectors = (stats.st_size + 511) / 512 in
  let size =
    match size_spec with
    | `All -> data_sectors
    | `Free free -> data_sectors + free
    | `Total size ->
      if size < data_sectors then begin
        Printf.eprintf "Partition size %d sectors for file '%S', but file is bigger" size part;
        exit 1
      end;
      size
  in
  part, size

let mbr_cat mbr_padding parts =
  let parts = List.map sectors parts in
  let partitions =
    List.fold_left
      (fun (offset, acc) (part, size) ->
         match Mbr.Partition.make ~ty:0x7F (Int32.of_int offset) (Int32.of_int size) with
         | Error e -> Printf.eprintf "Error creating partition for %s: %s" part e; exit 1
         | Ok partition -> offset + size, partition :: acc)
      (1 + mbr_padding, [])
      parts
    |> snd
  in
  let mbr =
    match Mbr.make partitions with
    | Error e -> Printf.eprintf "Partitioning error: %s" e; exit 2
    | Ok mbr -> mbr
  in
  let hdr = Cstruct.create 512 in
  Mbr.marshal hdr mbr;
  output_string stdout (Cstruct.to_string hdr);
  let empty_block = String.init 512 (Fun.const '\000') in
  for _ = 1 to mbr_padding do
    output_string stdout empty_block
  done;
  List.iter (fun (f, size) ->
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
      for _ = 1 to size - (written + 511) / 512 do
        output_string stdout empty_block
      done)
    parts

let jump mbr_padding parts =
  if List.length parts > 4
  then `Help (`Auto, None)
  else `Ok (mbr_cat mbr_padding parts)

open Cmdliner

let uint32_conv =
  let (let*) = Result.bind in
  let parse s =
    let* v = Arg.conv_parser Arg.int s in
    if 0xFFFF_FFFF land v <> v then
      Error (`Msg (Printf.sprintf "invalid value '%S', expected uint32 value" s))
    else Ok v
  and print = Arg.conv_printer Arg.int in
  Arg.conv (parse, print)

let part_conv =
  let docv = "PART[::[SIZE|+FREE]]" in
  let parse s =
    let (let*) = Result.bind in
    match String.split_on_char ':' s with
    | [ part ] ->
      let* part = Arg.conv_parser Arg.non_dir_file part in
      Ok (part, `All)
    | [ part ; "" ; sectors_s ] ->
      let* part = Arg.conv_parser Arg.non_dir_file part in
      let* sectors = Arg.conv_parser uint32_conv sectors_s in
      if sectors_s.[0] = '+' then
        Ok (part, `Free sectors)
      else
        Ok (part, `Total sectors)
    | _ ->
      Error (`Msg ("expected format is " ^ docv))
  in
  let print ppf = function
    | part, `All ->
      Arg.conv_printer Arg.non_dir_file ppf part
    | part, `Free free ->
      Format.fprintf ppf "%a::+%a"
        Arg.(conv_printer non_dir_file) part
        Arg.(conv_printer int) free
    | part, `Total size ->
      Format.fprintf ppf "%a::%a"
        Arg.(conv_printer non_dir_file) part
        Arg.(conv_printer int) size
  in
  Arg.conv ~docv (parse, print)

let parts =
  let doc = "Contents of the partition and optional size specification. \
             At most four partitions may be specified." in
  let docv = "PART[::[SIZE|+FREE]]" in
  Arg.(value & pos_all part_conv [] & info [] ~doc ~docv)

let mbr_padding =
  let doc = "Number of unused sectors after the Master Boot Record." in
  let docv = "MBR-PADDING" in
  Arg.(value & opt uint32_conv 0 & info ["mbr-padding"] ~doc ~docv)

let cmd =
  let doc = "Concatenate files into a MBR partitioned file" in
  let exits =
    Cmd.Exit.info 1 ~max:2 ~doc:"on partition error" ::
    Cmd.Exit.defaults
  in
  let info = Cmd.info "mbr-cat" ~version:"%%VERSION%%" ~doc ~exits in
  Cmd.v info Term.(ret (const jump $ mbr_padding $ parts))

let () = exit (Cmd.eval cmd)
