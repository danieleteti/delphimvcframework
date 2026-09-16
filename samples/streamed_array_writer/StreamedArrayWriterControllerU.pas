unit StreamedArrayWriterControllerU;

// ===========================================================================
//  Comprehensive showcase of every incremental (socket-level) streaming
//  mechanism in DelphiMVCFramework.
//
//  Three wire-format families:
//
//    TMVCJSONArrayWriter  - emits one valid JSON array, element by element,
//                          to the socket.  Four source variants:
//      /stream/dataset      forward-only DB cursor, raw record shape
//      /stream/dbobjects    forward-only DB cursor -> TPerson domain object
//      /stream/objectlist   in-memory TObjectList<TPerson>
//      /stream/enumeration  lazy TEnumerable<TPerson> backed by a file
//
//    TMVCSSEWriter        - text/event-stream (Server-Sent Events)
//      /stream/sse          one event per DB row; used as a live/push feed
//
//    TMVCJSONLWriter      - application/x-ndjson (JSON Lines / NDJSON)
//      /stream/jsonl        one JSON object per line, directly from DB cursor
//
//    TMVCCSVWriter        - text/csv
//      /stream/csv          one CSV row per DB row, header derived from RTTI
//
//  Two declarative / functional paths (no explicit writer code):
//      /stream/datasetfunc      return TDataSet    -> buffered, has Content-Length
//      /stream/datasetstreamed  return TMVCStreamedResponse -> chunked, no CL
//
//  All DB-backed procedures share the private OpenPeopleCursor helper so the
//  forward-only cursor setup is written exactly once.
//
//  NOTE: all streaming writers require an Indy-based backend.  This sample
//  uses Indy Direct, so every endpoint works.
// ===========================================================================

interface

uses
  Data.DB,
  FireDAC.Comp.Client,
  MVCFramework, MVCFramework.Commons;

type
  [MVCPath('/stream')]
  TStreamedArrayWriterController = class(TMVCController)
  private
    /// <summary>
    /// Opens a forward-only, read-only FireDAC cursor over the people table.
    /// AConn receives the newly created TFDConnection (caller must free both).
    /// Raises on connection or query error (both objects are freed on raise).
    /// </summary>
    function OpenPeopleCursor(out AConn: TFDConnection): TFDQuery;
  public
    // -----------------------------------------------------------------------
    //  TMVCJSONArrayWriter endpoints (four sources, one wire format)
    // -----------------------------------------------------------------------

    /// <summary>
    /// DATASET source: a forward-only FireDAC cursor (fmOnDemand +
    /// Unidirectional) streamed row by row. Neither the dataset nor the JSON
    /// is ever fully held in memory.
    /// </summary>
    [MVCPath('/dataset')]
    [MVCHTTPMethod([httpGET])]
    procedure StreamDataSet;

    /// <summary>
    /// DB-TO-OBJECT source: the same forward-only cursor, but each row is
    /// mapped to a TPerson domain object and that object is serialized (so the
    /// JSON follows the object's rules, not the raw column names). The object
    /// is created, emitted and freed inside the loop: one row + one object +
    /// one JSON value live at a time. This is the typical "hydrate an entity
    /// per row and stream it" pattern.
    /// </summary>
    [MVCPath('/dbobjects')]
    [MVCHTTPMethod([httpGET])]
    procedure StreamDBObjects;

    /// <summary>
    /// OBJECT-LIST source: a TObjectList&lt;TPerson&gt; you already have in
    /// memory, serialized one element at a time. The list itself is in memory
    /// (you built it), but the JSON payload is not - it goes to the socket
    /// element by element.
    /// </summary>
    [MVCPath('/objectlist')]
    [MVCHTTPMethod([httpGET])]
    procedure StreamObjectList;

    /// <summary>
    /// ENUMERATION source: a lazy object enumerator that reads a file one line
    /// at a time and yields one TPerson per line. The file is never loaded
    /// whole and at most one TPerson exists at any instant - the realistic
    /// "stream objects read from a file or a DB cursor" case. Works with a
    /// plain "for p in source do" loop.
    /// </summary>
    [MVCPath('/enumeration')]
    [MVCHTTPMethod([httpGET])]
    procedure StreamEnumeration;

    // -----------------------------------------------------------------------
    //  TMVCSSEWriter endpoint (Server-Sent Events, text/event-stream)
    // -----------------------------------------------------------------------

    /// <summary>
    /// SSE source: the same forward-only cursor, each row emitted as one SSE
    /// event named "person" whose data is the serialized row JSON and whose id
    /// is the row id. After the loop a final "done" event is sent.
    ///
    /// Note: SSE is normally used for live/push feeds (e.g. chat, dashboard
    /// updates). Here the writer is driven by a DB dataset to show the SSE
    /// wire format in isolation, driven from any iterable source.
    /// </summary>
    [MVCPath('/sse')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure StreamSSE;

    // -----------------------------------------------------------------------
    //  TMVCJSONLWriter endpoint (JSON Lines / NDJSON)
    // -----------------------------------------------------------------------

    /// <summary>
    /// JSON Lines source: one JSON object per line, each line self-contained
    /// and parseable independently. The format is common for log pipelines,
    /// bulk imports and streaming APIs that want line-delimited JSON rather
    /// than a top-level array.
    /// </summary>
    [MVCPath('/jsonl')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/x-ndjson')]
    procedure StreamJSONL;

    // -----------------------------------------------------------------------
    //  TMVCCSVWriter endpoint (text/csv)
    // -----------------------------------------------------------------------

    /// <summary>
    /// CSV source: one CSV row per DB row. The header is emitted once on the
    /// first Send() call, with column names derived from the TPerson RTTI
    /// properties (via the TClass overload). Each person object is created,
    /// sent and freed inside the loop.
    /// </summary>
    [MVCPath('/csv')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/csv')]
    procedure StreamCSV;

    // -----------------------------------------------------------------------
    //  Declarative / functional endpoints (no explicit writer loop)
    // -----------------------------------------------------------------------

    /// <summary>
    /// FUNCTIONAL-ACTION source (the zero-code path): just RETURN a TDataSet.
    /// The framework serializes it with the streaming JSON serializer (no JSON
    /// DOM, rows pulled on demand), so DB-side RAM stays bounded - BUT the full
    /// JSON payload is assembled in a memory stream and the response is sent
    /// WITH a Content-Length. This is "streaming serialization into a buffer",
    /// not record-by-record streaming to the socket. It works on every backend
    /// and needs no streaming code. Note the response here HAS Content-Length,
    /// unlike the explicit-writer endpoints above. Connection is request-scoped
    /// (provided by TMVCActiveRecordMiddleware), so it outlives the render.
    /// </summary>
    [MVCPath('/datasetfunc')]
    [MVCHTTPMethod([httpGET])]
    function GetPeopleDataSet: TDataSet;

    /// <summary>
    /// FUNCTIONAL-ACTION + true socket streaming: return TMVCStreamedResponse.
    /// Zero loop code AND flat framework RAM AND chunked transfer (keep-alive,
    /// works on every chunked-capable client). The best of /dataset (socket
    /// streaming) and /datasetfunc (declarative return).
    /// </summary>
    [MVCPath('/datasetstreamed')]
    [MVCHTTPMethod([httpGET])]
    function GetStreamedDataSet: TMVCStreamedResponse;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  FireDAC.Stan.Option,
  MVCFramework.ActiveRecord,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.SSE.Writer,
  PeopleSourcesU,
  StreamedArrayWriterConfigU;

const
  FIRST: array [0 .. 9] of string = ('Alice', 'Bob', 'Carol', 'Daniel', 'Eve',
    'Frank', 'Grace', 'Hannah', 'Ivan', 'Julia');
  LAST: array [0 .. 9] of string = ('Bianchi', 'Rossi', 'Verdi', 'Neri',
    'Gialli', 'Bruni', 'Romano', 'Ricci', 'Greco', 'Conti');
  COUNTRIES: array [0 .. 5] of string = ('IT', 'FR', 'DE', 'ES', 'NL', 'PT');

{ TStreamedArrayWriterController }

function TStreamedArrayWriterController.OpenPeopleCursor(out AConn: TFDConnection): TFDQuery;
begin
  AConn := TFDConnection.Create(nil);
  AConn.ConnectionDefName := CON_DEF_NAME;
  AConn.Open;
  Result := TFDQuery.Create(nil);
  try
    Result.Connection := AConn;
    Result.FetchOptions.Mode := TFDFetchMode.fmOnDemand;
    Result.FetchOptions.Unidirectional := True;
    Result.UpdateOptions.ReadOnly := True;
    Result.UpdateOptions.RequestLive := False;
    Result.Open('SELECT id, full_name, country, age FROM people ORDER BY id');
  except
    Result.Free;
    AConn.Free;
    raise;
  end;
end;

procedure TStreamedArrayWriterController.StreamDataSet;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lWriter: TMVCJSONArrayWriter;
begin
  lQry := OpenPeopleCursor(lConn);
  try
    lWriter := TMVCJSONArrayWriter.Create(Context);
    try
      while not lQry.Eof do
      begin
        if not lWriter.Connected then // client disconnected: stop early
          Break;
        lWriter.Send(Serializer.SerializeDataSetRecord(lQry));
        lQry.Next;
      end;
    finally
      lWriter.Free; // emits the closing "]" and closes the socket
    end;
  finally
    lQry.Free;
    lConn.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamDBObjects;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lWriter: TMVCJSONArrayWriter;
  lPerson: TPerson;
begin
  lQry := OpenPeopleCursor(lConn);
  try
    lWriter := TMVCJSONArrayWriter.Create(Context);
    try
      while not lQry.Eof do
      begin
        if not lWriter.Connected then
          Break;
        // Hydrate one domain object from the current row, serialize it with
        // the object's own rules, then discard it. Only one row, one object
        // and one JSON value are alive at any instant.
        lPerson := TPerson.Create(
          lQry.FieldByName('id').AsInteger,
          lQry.FieldByName('full_name').AsString,
          lQry.FieldByName('country').AsString,
          lQry.FieldByName('age').AsInteger);
        try
          lWriter.Send(Serializer.SerializeObject(lPerson));
        finally
          lPerson.Free;
        end;
        lQry.Next;
      end;
    finally
      lWriter.Free;
    end;
  finally
    lQry.Free;
    lConn.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamObjectList;
var
  lPeople: TObjectList<TPerson>;
  lWriter: TMVCJSONArrayWriter;
  lPerson: TPerson;
  I: Integer;
begin
  // A collection you already hold in memory (e.g. the result of a service
  // call). It is materialized in full, but we still stream the JSON element by
  // element rather than serializing the whole collection into one big string.
  lPeople := TObjectList<TPerson>.Create(True);
  try
    for I := 1 to 50000 do
      lPeople.Add(TPerson.Create(I, FIRST[I mod 10] + ' ' + LAST[(I div 10) mod 10],
        COUNTRIES[I mod 6], 18 + (I mod 62)));

    lWriter := TMVCJSONArrayWriter.Create(Context);
    try
      for lPerson in lPeople do
      begin
        if not lWriter.Connected then
          Break;
        lWriter.Send(Serializer.SerializeObject(lPerson));
      end;
    finally
      lWriter.Free;
    end;
  finally
    lPeople.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamEnumeration;
var
  lSource: TPersonFileSource;
  lWriter: TMVCJSONArrayWriter;
  lPerson: TPerson;
begin
  // Lazy source: the enumerator reads the feed file one line at a time and
  // yields one TPerson per line. Nothing is pre-loaded - neither the file nor
  // the objects nor the JSON ever exist in full. The same shape works for a
  // DB cursor, a network stream, etc.
  lSource := TPersonFileSource.Create(GetPeopleFeedPath);
  try
    lWriter := TMVCJSONArrayWriter.Create(Context);
    try
      for lPerson in lSource do
      begin
        if not lWriter.Connected then
          Break;
        lWriter.Send(Serializer.SerializeObject(lPerson));
      end;
    finally
      lWriter.Free;
    end;
  finally
    lSource.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamSSE;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lSSE: TMVCSSEWriter;
  lJSONSerializer: IMVCSerializer;
  lID: string;
begin
  // SSE is normally used for live/push feeds (chat, dashboards). Here the
  // writer is driven by a DB dataset to illustrate the SSE wire format with
  // any iterable source.
  //
  // Serializer for event payload: look up the JSON serializer explicitly
  // rather than using the no-arg Serializer shorthand, which resolves to
  // Serializer(GetContentType). Because [MVCProduces('text/event-stream')]
  // sets the response ContentType to 'text/event-stream' before the action
  // is invoked, the shorthand would raise "serializer not found" — no
  // serializer is registered for that MIME type. The JSON serializer is
  // the right choice anyway: SSE event data is JSON text; the outer
  // event-stream framing is handled by TMVCSSEWriter.
  lJSONSerializer := Serializer(TMVCMediaType.APPLICATION_JSON);
  lQry := OpenPeopleCursor(lConn);
  try
    lSSE := TMVCSSEWriter.Create(Context);
    try
      while not lQry.Eof do
      begin
        if not lSSE.Connected then
          Break;
        lID := lQry.FieldByName('id').AsString;
        lSSE.Send('person', lJSONSerializer.SerializeDataSetRecord(lQry), lID);
        lQry.Next;
      end;
      if lSSE.Connected then
        lSSE.Send('done', '');
    finally
      lSSE.Free;
    end;
  finally
    lQry.Free;
    lConn.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamJSONL;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lJSONL: TMVCJSONLWriter;
  lJSONSerializer: IMVCSerializer;
begin
  // Same serializer lookup pattern as StreamSSE: [MVCProduces('application/
  // x-ndjson')] sets the response ContentType before the action is invoked,
  // so the no-arg Serializer shorthand would fail to find a registered
  // serializer for that MIME type.
  lJSONSerializer := Serializer(TMVCMediaType.APPLICATION_JSON);
  lQry := OpenPeopleCursor(lConn);
  try
    lJSONL := TMVCJSONLWriter.Create(Context);
    try
      while not lQry.Eof do
      begin
        if not lJSONL.Connected then
          Break;
        lJSONL.Send(lJSONSerializer.SerializeDataSetRecord(lQry));
        lQry.Next;
      end;
    finally
      lJSONL.Free;
    end;
  finally
    lQry.Free;
    lConn.Free;
  end;
end;

procedure TStreamedArrayWriterController.StreamCSV;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lCSV: TMVCCSVWriter;
  lPerson: TPerson;
begin
  // Header columns are derived from TPerson RTTI properties (TClass overload).
  // Each person object is created, sent and freed inside the loop so only one
  // TPerson and one CSV row are live at a time. TMVCCSVWriter uses its own
  // internal CSV serializer, not the framework JSON serializer, so there is
  // no serializer lookup involved and no content-type issue.
  lCSV := TMVCCSVWriter.Create(Context, TPerson);
  try
    lQry := OpenPeopleCursor(lConn);
    try
      while not lQry.Eof do
      begin
        if not lCSV.Connected then
          Break;
        lPerson := TPerson.Create(
          lQry.FieldByName('id').AsInteger,
          lQry.FieldByName('full_name').AsString,
          lQry.FieldByName('country').AsString,
          lQry.FieldByName('age').AsInteger);
        try
          lCSV.Send(lPerson);
        finally
          lPerson.Free;
        end;
        lQry.Next;
      end;
    finally
      lQry.Free;
      lConn.Free;
    end;
  finally
    lCSV.Free;
  end;
end;

function TStreamedArrayWriterController.GetPeopleDataSet: TDataSet;
begin
  // Zero streaming code: return the dataset, the framework streams it for you.
  // SelectUnidirectionalDataSet keeps the DB cursor forward-only, so the DB
  // side stays bounded. The engine frees the returned dataset after rendering;
  // the AR middleware owns the request-scoped connection.
  Result := TMVCActiveRecord.SelectUnidirectionalDataSet(
    'SELECT id, full_name, country, age FROM people ORDER BY id', []);
end;

function TStreamedArrayWriterController.GetStreamedDataSet: TMVCStreamedResponse;
begin
  // Declarative + true socket streaming: wrap the dataset in TMVCStreamedResponse.
  // The framework flushes rows directly to the socket as chunked transfer
  // (Transfer-Encoding: chunked, no Content-Length), keeping framework-side
  // RAM flat for the full result set. The wrapper owns and frees the dataset
  // after rendering; the AR middleware keeps the connection alive for the
  // duration of the request.
  //
  // NOTE: TMVCRenderer(Self) is used to disambiguate from the local
  // "procedure StreamDataSet" endpoint that has the same name.
  Result := TMVCRenderer(Self).StreamDataSet(
    TMVCActiveRecord.SelectUnidirectionalDataSet(
      'SELECT id, full_name, country, age FROM people ORDER BY id', []),
    ncLowerCase, True);
end;

end.
