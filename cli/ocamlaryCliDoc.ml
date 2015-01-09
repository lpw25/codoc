(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open OcamlaryCli

module StringMap = Map.Make(String)

let doc_xml_parser = DocOckXmlParse.build (fun input ->
  match Xmlm.input_tree
    ~el:OcamlaryDoc.root_of_xml
    ~data:OcamlaryDoc.data_of_xml
    input
  with None -> failwith "can't find root" (* TODO: fixme *)
  | Some root -> root
)

let xml_error xml_file ?start (line,col) s = match start with
  | Some (start_line, start_col) ->
    Printf.eprintf "\n%s line %d column %d - line %d column %d:\n%s\n\n"
      xml_file start_line start_col line col s
  | None ->
    Printf.eprintf "\n%s line %d column %d:\n%s\n\n" xml_file line col s

module LinkIndex = struct (* TODO: use digest, too *)
  open OcamlaryDoc
  type t = {
    root_by_name : (string, root Lazy.t) Hashtbl.t;
    unit_by_root : (root, root DocOckTypes.Unit.t) Hashtbl.t;
  }

  let root_by_name idx name =
    try Some (Lazy.force (Hashtbl.find idx.root_by_name name))
    with Not_found -> None

  let unit_by_root idx root =
    try Hashtbl.find idx.unit_by_root root
    with Not_found ->
      let name = OcamlaryDoc.Maps.name_of_root root in
      failwith ("couldn't find unit for root "^name) (* TODO *)

  let unit_by_name idx name = match root_by_name idx name with
    | None -> failwith ("couldn't find unit for name "^name) (* TODO *)
    | Some root -> unit_by_root idx root

  let index idx name root unit =
    Hashtbl.replace idx.root_by_name name (Lazy.from_val root);
    Hashtbl.replace idx.unit_by_root root unit

  let rec index_units idx path doc_index =
    StringMap.iter (fun name doc ->
      Hashtbl.replace idx.root_by_name name
        (Lazy.from_fun (fun () ->
          let xml_file = Filename.concat path doc.OcamlaryIndex.xml_file in
          let ic = open_in xml_file in
          let input = Xmlm.make_input (`Channel ic) in
          match DocOckXmlParse.file doc_xml_parser input with
          | DocOckXmlParse.Error (start, pos, s) ->
            close_in ic;
            (* TODO: fixme? different/better error style? *)
            xml_error xml_file ?start pos s;
            exit 1
          | DocOckXmlParse.Ok unit ->
            match OcamlaryDoc.Maps.root_of_ident
              (DocOckPaths.Identifier.any unit.DocOckTypes.Unit.id) with
              | Some (root, mod_name) ->
                Hashtbl.replace idx.unit_by_root root unit;
                root
              | None -> (* TODO: fixme *) failwith "missing root"
         ))
    ) doc_index.OcamlaryIndex.units;
    StringMap.iter (fun name pkg ->
      let index_path = Filename.concat path pkg.OcamlaryIndex.index in
      let path = Filename.concat path pkg.OcamlaryIndex.pkg_name in
      let index = OcamlaryIndex.read index_path in
      index_units idx path index
    ) doc_index.OcamlaryIndex.pkgs

  let create path doc_index =
    let idx = {
      root_by_name = Hashtbl.create 10;
      unit_by_root = Hashtbl.create 10;
    } in
    index_units idx path doc_index;
    idx
end

let read_cmti root path = DocOck.(match read_cmti root path with
  | Not_an_interface -> failwith (path^" is not an interface") (* TODO *)
  | Wrong_version_interface ->
    failwith (path^" has the wrong format version") (* TODO *)
  | Corrupted_interface -> failwith (path^" is corrupted") (* TODO *)
  | Not_a_typedtree -> failwith (path^" is not a typed tree") (* TODO *)
  | Ok unit -> unit
)

let read root = read_cmti root OcamlaryDoc.Root.(to_path (to_source root))

let read_and_index index (root, file) =
  let mod_name = OcamlaryDoc.Maps.name_of_root root in
  let unit = read root in
  LinkIndex.index index mod_name root unit;
  (mod_name, file)

let resolver failure_set index = DocOckResolve.build_resolver
  (fun _req_unit mod_name -> match LinkIndex.root_by_name index mod_name with
  | Some root -> Some root
  | None -> Hashtbl.replace failure_set mod_name (); None (* TODO *)
  )
  (LinkIndex.unit_by_root index)

let xml index mod_name xml_file = (* TODO: mark the root for "this"? *)
  let unit = LinkIndex.unit_by_name index mod_name in
  let failures = Hashtbl.create 10 in
  let unit = DocOckResolve.resolve (resolver failures index) unit in
  let issues = Hashtbl.fold (fun name () issues ->
    (OcamlaryIndex.Module_resolution_failed name)::issues
  ) failures [] in
  let out_file = open_out xml_file in
  let output = Xmlm.make_output (`Channel out_file) in
  let printer = DocOckXmlPrint.build (fun output root ->
    Xmlm.output_tree (fun x -> x) output (List.hd (OcamlaryDoc.xml_of_root root))
  ) in
  DocOckXmlPrint.file printer output unit;
  close_out out_file;
  issues

let html xml_file html_file =
  let in_file = open_in xml_file in
  let input = Xmlm.make_input (`Channel in_file) in
  match DocOckXmlParse.file doc_xml_parser input with
  | DocOckXmlParse.Error (start, pos, s) ->
    close_in in_file;
    [OcamlaryIndex.Xml_error (xml_file, s)]
  | DocOckXmlParse.Ok unit ->
    close_in in_file;
    let pathloc = OcamlaryDocHtml.pathloc (* TODO: fixme *)
      ~unit:unit
      ~index:(fun root -> (* TODO: report failures *)
        Some (Uri.of_string (OcamlaryDoc.Root.to_path root))
      )
    in
    let html =
      OcamlaryDocHtml.of_unit ~pathloc unit
    in
  (* TODO: fixme *)
    let html = <:html<<html><head><meta charset="utf-8"/><link rel="stylesheet" type="text/css" href="file:///home/dsheets/Code/ocamlary/share/ocamlary.css"/></head><body>$html$</body></html>&>> in
    let out_file = open_out html_file in
    output_string out_file "<!DOCTYPE html>\n";
    let output = Xmlm.make_output ~decl:false (`Channel out_file) in
    Htmlm.Xhtmlm.output_doc_tree output (List.hd html);
    close_out out_file;
    [] (* TODO: issues *)

let only_cmti f file path output =
  if Filename.check_suffix file ".cmti"
  then begin
    Printf.eprintf "%s\n%!" path;
    f file path output
  end
  else false

open Webmaster_cli

let resolve_path base rel =
  Uri.(to_string (resolve "" (of_string base) (of_string rel)))

let depth path =
  max 0 (List.length (Stringext.split path ~on:'/') - 1)

let rec ascent_of_depth tl = function
  | 0 -> tl
  | n -> ascent_of_depth ("../" ^ tl) (n - 1)

let rel_of_path depth path = (ascent_of_depth "" depth) ^ path

let cmti_path path _output = path

let resource_of_cmti output = Filename.(
  if check_suffix output ".cmti"
  then chop_suffix output ".cmti"
  else output
)

let xml_path output =
  let base_name = resource_of_cmti output in
  rel_of_path (depth output) (base_name ^ ".xml")

let html_path output =
  let base_name = resource_of_cmti output in
  rel_of_path (depth output) (base_name ^ ".html")

let generate ({ force }) formats (_output_links,output) (_path_links,path) pkg =
  let cmd = "doc" in
  let output_type = Webmaster_file.output_type path output in
  let doc_index_path, doc_index = match output_type with
    | Some (`Dir output) -> output, OcamlaryIndex.(read (index_file output))
    | Some (`File _) | None -> "", OcamlaryIndex.empty
  in
  let ((pkg_path, pkg_index_path), pkg_index), pkg_parents =
    OcamlaryIndex.traverse doc_index_path pkg
  in
  let index = LinkIndex.create doc_index_path doc_index in
  let roots = ref [] in
  let record file path output =
    let mod_name = FindlibUnits.unit_name_of_path path in
    let pkg_file = Filename.concat pkg file in
    let root = OcamlaryDoc.(
      Html (html_path pkg_file,
            Xml (xml_path pkg_file,
                 Cmti (cmti_path path output,
                       mod_name)
            )
      )
    ) in
    roots := (root,file) :: !roots;
    false
  in
  let ret =
    Webmaster_file.output_of_input ~force ~cmd (only_cmti record) path
      (match output, output_type with
      | `Dir _, _ | `Missing _, Some (`Dir _) ->
        Webmaster_file.ensure_directory_exists ~perm:0o700 pkg_path;
        `Dir pkg_path
      | `File _, _ | `Missing _, (Some (`File _) | None) -> output
      )
  in
  match ret with
  | `Ok () ->
    let units = List.map (read_and_index index) !roots in
    let gunits =
      List.fold_left (fun gunits (name, file) ->
        let base_name = Filename.concat pkg_path (resource_of_cmti file) in
        let xml_file = base_name ^ ".xml" in
        let html_file = base_name ^ ".html" in
        let xml_issues = xml index name xml_file in
        let html_issues = html xml_file html_file in
        let local_resource = resource_of_cmti file in
        { OcamlaryIndex.mod_name = name;
          xml_file = local_resource ^ ".xml";
          html_file = Some (local_resource ^ ".html");
          issues=html_issues @ xml_issues;
        } :: gunits
      ) [] units
    in

    begin match output_type with
    | Some (`Dir output) ->
      let open OcamlaryIndex in
      let unit_index = List.fold_left (fun map unit ->
        StringMap.add unit.mod_name unit map
      ) pkg_index.units gunits in
      write (Filename.concat doc_index_path pkg_index_path)
        { pkg_index with units = unit_index };

      List.iter (fun ((_, index_path), index) ->
        write index_path index
      ) pkg_parents
    | Some (`File _) | None -> ()
    end;

    let warns = List.fold_left (fun err gunit ->
      (List.length gunit.OcamlaryIndex.issues <> 0) || err
    ) false gunits in
    `Ok (Webmaster_file.check ~cmd warns)
  | ret -> ret