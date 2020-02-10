
module SU = Smtlib_utils.V_2_6
module P = Lsp.Protocol

let section = "smtlib-lsp"

module State = struct
  module TD = Lsp.Text_document

  type uri = Lsp.Uri.t
  type doc = TD.t

  module Uri_map = CCMap.Make(struct
      type t = uri
      let compare a b = String.compare (Lsp.Uri.to_path a)(Lsp.Uri.to_path b)
    end)

  type t = {
    docs: doc Uri_map.t;
    as_ast: (SU.Ast.statement list, SU.Loc.t option * string) result Uri_map.t;
  }

  let empty : t = { docs=Uri_map.empty; as_ast=Uri_map.empty }

  let parse_doc_ (s:string) : (_, _) result =
    try
      Ok (SU.parse_string_exn s)
    with
    | SU.Ast.Parse_error (loc, msg) ->
      Error (loc, msg)

  let range_of_loc (l:SU.Loc.t) : P.Range.t =
    {P.Range.
      start_= {P.Position.character=l.start_column-1; line=l.start_line-1};
      end_={P.Position.character=l.stop_column-1; line=l.stop_line-1};
    }

  let on_initialize _rpc st (params:P.Initialize.params) =
    Lsp.Logger.log ~section ~title:"on-req" "";
    let r = {
      P.Initialize.server_capabilities={
        P.empty_server_capabilities
        with hoverProvider=false;
      }} in
    Ok (st, r)

  let on_request _rpc st _cap r =
    Lsp.Logger.log ~section ~title:"on-req" "";
    Error "not implemented"

  let send_diags rpc doc as_ast =
    begin match as_ast with
      | Ok _ ->
        Lsp.Logger.log ~section ~title:"docDidOpen" "doc validated";
        (* clear diagnostics *)
        Lsp.Rpc.send_notification rpc @@
        Lsp.Rpc.Server_notification.PublishDiagnostics
          {uri=TD.documentUri doc; diagnostics=[]};
        ()
      | Error (locopt, msg) ->
        Lsp.Logger.log ~section ~title:"docDidOpen" "error: %s" msg;
        (* TODO: also publish warnings on shadowing, this kind of stuff *)
        let diag: P.PublishDiagnostics.diagnostic = {
          P.PublishDiagnostics.
          range=
            (match locopt with
             | Some loc -> range_of_loc loc
             | None ->
               {P.start_={line=0;character=0};end_={line=1;character=1}});
          severity=Some Error;
          code=NoCode; source=Some "smtlib-utils"; message=msg;
          relatedInformation=[]; relatedLocations=[];
        } in
        Lsp.Rpc.send_notification rpc @@
        Lsp.Rpc.Server_notification.PublishDiagnostics
          {uri=TD.documentUri doc; diagnostics=[diag]};
    end

  let on_notification rpc (self:t) notif =
    let module N = Lsp.Rpc.Client_notification in
    match notif with
    | N.Exit -> Ok self
    | TextDocumentDidOpen { textDocument=d } ->
      Lsp.Logger.log ~section ~title:"docDidOpen" "uri %s, size %d"
        (Lsp.Uri.to_path d.uri) (String.length d.text);
      let doc = TD.make ~version:d.version d.uri d.text in
      let as_ast=parse_doc_ (TD.text doc) in
      send_diags rpc doc as_ast;
      Ok {
        docs=Uri_map.add d.uri doc self.docs;
        as_ast=Uri_map.add d.uri as_ast self.as_ast;
      }
    | N.TextDocumentDidChange { textDocument=d; contentChanges; } ->
      begin match Uri_map.find d.uri self.docs with
        | doc ->
          let doc =
            List.fold_right
              (TD.apply_content_change ~version:d.version)
              contentChanges doc
          in
          let as_ast=parse_doc_ (TD.text doc) in
          send_diags rpc doc as_ast;
          Ok {
            docs=Uri_map.add d.uri doc self.docs;
            as_ast=Uri_map.add d.uri as_ast self.as_ast;
          }
        | exception Not_found ->
          Error "no such document found"
      end
    | N.Initialized ->
      Lsp.Logger.log ~section ~title:"initialized" "👍"; Ok self
    | N.UnknownNotification (s, _) ->
      Lsp.Logger.log ~section ~title:"on-notif" "unknown notif %s" s;
      Error "not implemented"

  let handler : t Lsp.Rpc.handler = {
    on_initialize;
    on_request;
    on_notification;
  }
end


let main () =
  Lsp.Logger.log ~section:"" ~title:"app" "start lsp";
  let handler = State.handler in
  Lsp.Rpc.start State.empty handler stdin stdout;
  Lsp.Logger.log ~section:"" ~title:"app" "stop lsp";
  ()

let () =
  Lsp.Logger.with_log_file (Some "/tmp/lsp.log") main
