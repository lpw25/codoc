(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

module BlueTree = Blueprint.Tree

module Error = CodocCli.Error
module Dir = CodocSysUtil.Dir

type file_type = CodocSysUtil.file_type = Interface | Index | Unknown

let (/) = Filename.concat

let share_dir = ref CodocConfig.share_dir

let index_template = lazy CodocTemplate.(load !share_dir index)

let interface_template = lazy CodocTemplate.(load !share_dir interface)

let html_name_of = CodocUnit.Href.html_name_of

let write_html ~force ~css ~title html_file templ path =
  let vars = Blueprint.Tree.(of_kv_maybe [
    "css", Some (of_string css);
    "title", match title with None -> None | Some s -> Some (of_string s);
  ]) in
  let vars = Blueprint.Scope.overlay vars templ in
  let templ = Blueprint.Scope.(match find templ path with
    | None -> Printf.eprintf "template path '%s' missing" path; exit 1
    | Some t -> t
  ) in
  let template = Blueprint.(default_rope (Scope.template templ)) in
    if not force && Sys.file_exists html_file
    then [], [Error.use_force html_file]
    else match Dir.make_exist ~perm:0o755 (Filename.dirname html_file) with
    | Some err -> [], [err]
    | None -> begin
        match Blueprint_unix.bind_to_file html_file vars template with
        | () -> [], []
        | exception Blueprint.Error err ->
          [ CodocIndex.Template_error (Blueprint.error_message err) ], []
      end

let update_css id css = CodocUnit.Href.ascent_of_ident id ^ css

let issues_of_doc_errors = List.map (fun x -> CodocIndex.Doc_error x)

let title_of_id id =
  let name = DocOck.Paths.Identifier.name id in
  let path = CodocDoc.Maps.string_of_ident id in
  Printf.sprintf "%s (%s)" name path

let write_interface ~force ~unit_file ~css (id, uri, html, issues) =
  let html = Blueprint.Tree.of_cons "data" html in
  let template = Lazy.force interface_template in
  let html = Blueprint.Scope.overlay html template in
  let title = Some (title_of_id id) in
  let css = update_css id css in
  let html_path = Uri.(resolve "" (of_string unit_file) uri) in
  let html_file = Uri.to_string html_path in
  let write_issues, errs =
    write_html ~force ~css ~title html_file html "interface"
  in
  let name = DocOck.Paths.Identifier.name id in
  let file = Uri.to_string uri in
  name, file, issues @ write_issues, errs

let add_up ~loc tree = match CodocUnit.Href.up loc with
  | Some up_href ->
    let up_href = Uri.to_string up_href in
    BlueTree.(add "up" (of_cons "href" (of_string up_href)) tree)
  | None -> tree

let module_interface ~scheme ~env ~pkg_root ~unit_loc m =
  let id = DocOck.(Paths.Identifier.any m.Types.Module.id) in
  let doc_errors = CodocAnalysis.of_module m in
  let issues = issues_of_doc_errors doc_errors in
  let loc =
    match CodocUnit.Href.loc ?pkg_root scheme id with
    | None -> failwith "invariant violation module_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let body = CodocDocHtml.of_module loc env m in
  let html = add_up ~loc (BlueTree.of_cons "module" body) in
  let html = BlueTree.root html in
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation module_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  (id, uri, html, issues)

let module_type_interface ~scheme ~env ~pkg_root ~unit_loc m =
  let id = DocOck.(Paths.Identifier.any m.Types.ModuleType.id) in
  let doc_errors = CodocAnalysis.of_module_type m in
  let issues = issues_of_doc_errors doc_errors in
  let loc =
    match CodocUnit.Href.loc ?pkg_root scheme id with
    | None -> failwith "invariant violation module_type_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let body = CodocDocHtml.of_module_type loc env m in
  let html = add_up ~loc (BlueTree.of_cons "module-type" body) in
  let html = BlueTree.root html in
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation module_type_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  (id, uri, html, issues)

let class_interface ~scheme ~env ~pkg_root ~unit_loc c =
  let id = DocOck.(Paths.Identifier.any c.Types.Class.id) in
  let doc_errors = CodocAnalysis.of_class c in
  let issues = issues_of_doc_errors doc_errors in
  let loc =
    match CodocUnit.Href.loc ?pkg_root scheme id with
    | None -> failwith "invariant violation class_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let body = CodocDocHtml.of_class loc env c in
  let html = add_up ~loc (BlueTree.of_cons "class" body) in
  let html = BlueTree.root html in
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation class_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  (id, uri, html, issues)

let class_type_interface ~scheme ~env ~pkg_root ~unit_loc c =
  let id = DocOck.(Paths.Identifier.any c.Types.ClassType.id) in
  let doc_errors = CodocAnalysis.of_class_type c in
  let issues = issues_of_doc_errors doc_errors in
  let loc =
    match CodocUnit.Href.loc ?pkg_root scheme id with
    | None -> failwith "invariant violation class_type_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let body = CodocDocHtml.of_class_type loc env c in
  let html = add_up ~loc (BlueTree.of_cons "class-type" body) in
  let html = BlueTree.root html in
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation class_type_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  (id, uri, html, issues)

let unit_interface ~scheme ~env ~pkg_root ~unit_loc u =
  let id = DocOck.(Paths.Identifier.any u.Types.Unit.id) in
  let doc_errors = CodocAnalysis.of_unit u in
  let issues = issues_of_doc_errors doc_errors in
  let loc =
    match CodocUnit.Href.loc ?pkg_root scheme id with
    | None -> failwith "invariant violation unit_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let body = CodocDocHtml.of_unit loc env u in
  let html = add_up ~loc (BlueTree.of_cons "module" body) in
  let html = BlueTree.root html in
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation unit_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  (id, uri, html, issues)

let rec render_module ~force ~scheme ~unit_file
                      ~pkg_root ~css ~env ~unit_loc m =
  let intf = module_interface ~scheme ~env ~pkg_root ~unit_loc m in
  let name, file, issues, errs =
    write_interface ~force ~unit_file ~css intf
  in
  let expander = CodocEnvironment.expander env in
  let children, child_errs =
    match DocOck.expand_module expander m with
    | None -> [], []
    | Some (DocOck.Signature sg) ->
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc sg
    | Some (DocOck.Functor(args, sg)) ->
      (* TODO render args as well *)
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc sg
  in
  CodocIndex.Module(name, file, issues, children), errs @ child_errs

and render_module_type ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc m =
  let intf = module_type_interface ~scheme ~env ~pkg_root ~unit_loc m in
  let name, file, issues, errs =
    write_interface ~force ~unit_file ~css intf
  in
  let expander = CodocEnvironment.expander env in
  let children, child_errs =
    match DocOck.expand_module_type expander m with
    | None -> [], []
    | Some (DocOck.Signature sg) ->
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc sg
    | Some (DocOck.Functor(args, sg)) ->
      (* TODO render args as well *)
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc sg
  in
  CodocIndex.ModuleType(name, file, issues, children), errs @ child_errs

and render_class ~force ~scheme ~unit_file
                 ~pkg_root ~css ~env ~unit_loc c =
  let intf = class_interface ~scheme ~env ~pkg_root ~unit_loc c in
  let name, file, issues, errs =
    write_interface ~force ~unit_file ~css intf
  in
  CodocIndex.Class(name, file, issues), errs

and render_class_type ~force ~scheme ~unit_file
                      ~pkg_root ~css ~env ~unit_loc c =
  let intf = class_type_interface ~scheme ~env ~pkg_root ~unit_loc c in
  let name, file, issues, errs =
    write_interface ~force ~unit_file ~css intf
  in
  CodocIndex.ClassType(name, file, issues), errs

and render_unit ~force ~scheme ~unit_file
                ~pkg_root ~css ~env ~unit_loc u =
  let intf = unit_interface ~scheme ~env ~pkg_root ~unit_loc u in
  let name, file, issues, errs =
    write_interface ~force ~unit_file ~css intf
  in
  let expander = CodocEnvironment.expander env in
  let children, child_errs =
    match DocOck.expand_unit expander u with
    | None -> [], []
    | Some sg ->
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc sg
  in
  CodocIndex.Module(name, file, issues, children), errs @ child_errs

and render_signature ~force ~scheme ~unit_file ~pkg_root ~css ~env ~unit_loc =
  let open DocOck.Types.Signature in function
    | Module m :: rest ->
      let m, errs =
        render_module ~force ~scheme ~unit_file
                      ~pkg_root ~css ~env ~unit_loc m
      in
      let rest, rest_errs =
        render_signature ~force ~scheme ~unit_file
                         ~pkg_root ~css ~env ~unit_loc rest
      in
        m :: rest, errs @ rest_errs
    | ModuleType m :: rest ->
      let m, errs =
        render_module_type ~force ~scheme ~unit_file
                           ~pkg_root ~css ~env ~unit_loc m
      in
      let rest, rest_errs =
        render_signature ~force ~scheme ~unit_file
                         ~pkg_root ~css ~env ~unit_loc rest
      in
        m :: rest, errs @ rest_errs
    | Class c :: rest ->
      let c, errs =
        render_class ~force ~scheme ~unit_file
                     ~pkg_root ~css ~env ~unit_loc c
      in
      let rest, rest_errs =
        render_signature ~force ~scheme ~unit_file
                         ~pkg_root ~css ~env ~unit_loc rest
      in
        c :: rest, errs @ rest_errs
    | ClassType c :: rest ->
      let c, errs =
        render_class_type ~force ~scheme ~unit_file
                          ~pkg_root ~css ~env ~unit_loc c
      in
      let rest, rest_errs =
        render_signature ~force ~scheme ~unit_file
                         ~pkg_root ~css ~env ~unit_loc rest
      in
        c :: rest, errs @ rest_errs
    | Include incl :: rest -> begin
        let expander = CodocEnvironment.expander env in
        match DocOck.expand_include expander incl with
        | None ->
          render_signature ~force ~scheme ~unit_file
                           ~pkg_root ~css ~env ~unit_loc rest
        | Some sg ->
          render_signature ~force ~scheme ~unit_file
                           ~pkg_root ~css ~env ~unit_loc (sg @ rest)
      end
    | _ :: rest ->
      render_signature ~force ~scheme ~unit_file
                       ~pkg_root ~css ~env ~unit_loc rest
    | [] -> [], []

let read_unit in_file =
  let ic = open_in in_file in
  let input = Xmlm.make_input (`Channel ic) in
  match DocOckXmlParse.file CodocXml.doc_parser input with
  | DocOckXmlParse.Error (start, pos, s) ->
    close_in ic;
    let issue = CodocIndex.Xml_error (in_file, pos, s) in
    `Error [ CodocIndex.error_of_unit_issue in_file issue ]
  | DocOckXmlParse.Ok (unit : _ DocOck.Types.Unit.t)->
    close_in ic;
    `Ok unit

let render_interface_ok ~force in_file unit_file scheme css =
  match read_unit in_file with
  | `Error errs -> CodocCli.combine_errors errs
  | `Ok unit ->
    let env = CodocEnvironment.create_for_unit unit in
    let unit_loc =
      let id = DocOck.Paths.Identifier.any unit.DocOck.Types.Unit.id in
      match CodocUnit.Href.loc "file" id with
      | None -> failwith "invariant violation render_dir" (* TODO: ? *)
      | Some loc -> loc
    in
    let html_files, errs =
        render_unit ~force ~scheme ~unit_file
                    ~pkg_root:None ~css ~env ~unit_loc unit
    in
    match errs with
    | [] ->
        CodocIndex.print_html_file_issues html_files;
        `Ok ()
    | errs -> CodocCli.combine_errors errs

let render_index ~force name index out_file scheme css =
  let html =
    CodocIndexHtml.of_package ~name ~index ~scheme
  in
  let html = Blueprint.Tree.of_cons "data" html in
  let html = Blueprint.Scope.overlay html (Lazy.force index_template) in
  (* TODO: fixme title *)
  let title = if name = "" then None else Some name in
  let issues, errs = write_html ~force ~css ~title out_file html "index" in
  match errs with
  | [] ->
      CodocIndex.print_issues out_file issues;
      `Ok ()
  | errs -> CodocCli.combine_errors errs

module StringMap = Map.Make(String)

let check_create_safe ~force index out_dir =
  let open CodocIndex in
  fold_down
    ~unit_f:(fun r index ({ xml_file; }) ->
      let html_file = html_name_of xml_file in
      let path = match Filename.dirname index.path with "." -> "" | p -> p in
      let xml_file = out_dir / path / xml_file in
      let path = out_dir / path / html_file in
      match read_unit xml_file with
      | `Error es -> begin match r with
        | `Error errs -> `Error (es@errs)
        | `Ok _ -> `Error es
      end
      | `Ok unit -> match r with
        | `Error errs -> `Error errs
        | `Ok others -> `Ok (StringMap.add path unit others)
    )
    ~pkg_f:(fun rc r index ->
      let html_file = html_name_of index.path in
      let path = out_dir / html_file in
      if not force && Sys.file_exists path
      then begin match r with
        | `Error errs -> rc (`Error ((Error.use_force path)::errs))
        | `Ok _ -> rc (`Error [ Error.use_force path ])
      end
      else match Dir.make_exist ~perm:0o755 (Filename.dirname path) with
        | None -> rc r
        | Some err -> match r with
          | `Error errs -> `Error (err::errs) (* don't recurse *)
          | `Ok _ -> `Error [ err ]
    )
    (`Ok StringMap.empty) index

let run_index ~force ~index in_index out_dir package scheme css =
  let root = Filename.dirname in_index in
  let path = Filename.basename in_index in
  let idx = CodocIndex.read root path in
  let env = CodocEnvironment.create idx in
  let pkg_index = CodocIndex.goto idx package in
  let units = check_create_safe ~force pkg_index out_dir in
  match units with
  | `Error errs -> CodocCli.combine_errors errs
  | `Ok units ->
    let open CodocIndex in
    let errors = ref [] in
    let unit_f idxs idx gunit =
      let path = match Filename.dirname idx.path with "." -> "" | p -> p in
      let html_file = html_name_of gunit.xml_file in
      let pkg_root = Some CodocUtil.(ascent_of_depth "" (depth html_file)) in
      let html_path = path / html_file in
      let css = CodocUtil.(ascent_of_depth css (depth html_path)) in
      let unit_file = out_dir / html_path in
      let unit = StringMap.find unit_file units in (* TODO: shouldn't exn... *)
      let unit_loc =
        let id = DocOck.Paths.Identifier.any unit.DocOck.Types.Unit.id in
        match CodocUnit.Href.loc ?pkg_root "file" id with
        | None -> failwith "invariant violation render_dir" (* TODO: ? *)
        | Some loc -> loc
      in
      let html_files, errs =
        render_unit ~force ~scheme ~unit_file
                    ~pkg_root ~css ~env ~unit_loc unit
      in
      errors := errs @ !errors;
      if index
      then
        let out_index = read_cache { idx with root = out_dir } idx.path in
        let html_files = Some html_files in
        let index = set_gunit out_index { gunit with html_files } in
        write_cache index;
        idxs
      else (CodocIndex.print_html_file_issues html_files; idxs)

    in
    let pkg_f rc idxs idx = rc (idx::idxs) in
    let idxs = fold_down ~unit_f ~pkg_f [] pkg_index in
    match !errors with
    | [] ->
      if index then begin
        (* TODO: errors? XML errors? *)
        List.iter (fun idx ->
          let idx = read_cache { idx with root = out_dir } idx.path in
          let html_file = html_name_of idx.path in
          let path = out_dir / html_file in
          let name = match Filename.dirname idx.path with
            | "." -> ""
            | dir -> dir
          in
          let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
          (* TODO: errors? *)
          ignore (render_index ~force name idx path scheme css);
          ()
        ) idxs;
        let parents = CodocIndex.diff idx package in
        List.iter (fun idx ->
          let index = read_cache { idx with root = out_dir } idx.path in
          write_cache index;
          let html_file = html_name_of idx.path in
          let path = out_dir / html_file in
          let name = match Filename.dirname idx.path with
            | "." -> ""
            | dir -> dir
          in
          let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
          (* TODO: errors? *)
          ignore (render_index ~force name idx path scheme css);
          ()
        ) parents;
        flush_cache idx;
      end;
      `Ok ()
    | errs -> CodocCli.combine_errors errs

let maybe_copy ~force path target_dir =
  let file_name = Filename.basename path in
  let target = target_dir / file_name in
  if not force && Sys.file_exists target
  then
    let path_digest = Digest.file path in
    let target_digest = Digest.file target in
    if path_digest <> target_digest
    then Error.use_force target
    else `Ok file_name
  else
    match Dir.make_exist ~perm:0o755 target_dir with
    | Some err -> err
    | None ->
      CodocSysUtil.copy path target;
      `Ok file_name

let shared_css share = share / CodocConfig.css_name

let render_with_css ~force share css_dir render_f = function
  | Some css -> render_f (Uri.to_string css)
  | None ->
    let css = shared_css share in
    match maybe_copy ~force css css_dir with
    | `Ok css -> render_f css
    | `Error _ as err -> err

let render_file ~force in_file out_file scheme css share =
  let out_dir = Filename.dirname out_file in
  let render_f = render_interface_ok ~force in_file out_file scheme in
  render_with_css ~force share out_dir render_f css

let run ({ CodocCli.Common.force; index }) output path package scheme css share =
  share_dir := share;
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface ->
        let html_file = html_name_of in_file in
        render_file ~force in_file html_file scheme css share
      | Index ->
        let out_dir = Filename.dirname in_file in
        let render_f =
          run_index ~force ~index in_file out_dir package scheme
        in
        render_with_css ~force share out_dir render_f css
    end
  | `File in_file, Some (`Missing out_file) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface -> render_file ~force in_file out_file scheme css share
      | Index ->
        let render_f =
          run_index ~force ~index in_file out_file package scheme
        in
        render_with_css ~force share out_file render_f css
    end
  | `File in_file, Some (`File out_file) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface -> render_file ~force in_file out_file scheme css share
      | Index -> Error.index_to_file in_file out_file
    end
  | `File in_file, Some (`Dir out_dir) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface ->
        let base_name = Filename.basename in_file in
        let html_name = html_name_of base_name in
        render_file ~force in_file (out_dir / html_name) scheme css share
      | Index ->
        let render_f =
          run_index ~force ~index in_file out_dir package scheme
        in
        render_with_css ~force share out_dir render_f css
    end
  | `Dir in_dir, None -> begin
      match CodocSysUtil.search_for_source in_dir with
      | None -> Error.source_not_found in_dir
      | Some (source, Unknown) -> Error.unknown_file_type source
      | Some (source, Interface) ->
        let html_file = html_name_of source in
        render_file ~force source html_file scheme css share
      | Some (source, Index) ->
        let render_f =
          run_index ~force ~index source in_dir package scheme
        in
        render_with_css ~force share in_dir render_f css
    end
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let base_name = Filename.basename source in
      let html_name = html_name_of base_name in
      render_file ~force source (out_dir / html_name) scheme css share
    | Some (source, Index) ->
      let render_f =
        run_index ~force ~index source out_dir package scheme
      in
      render_with_css ~force share out_dir render_f css
    end
  | `Dir in_dir, Some (`File out_file) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      render_file ~force source out_file scheme css share
    | Some (source, Index) -> Error.index_to_file source out_file
    end
