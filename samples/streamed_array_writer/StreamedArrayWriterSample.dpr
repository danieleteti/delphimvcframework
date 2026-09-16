program StreamedArrayWriterSample;

// ===========================================================================
//  Comprehensive showcase of every incremental (socket-level) streaming
//  mechanism in DelphiMVCFramework.
//
//  Three wire-format families, nine endpoints total:
//
//    TMVCJSONArrayWriter  application/json       (well-formed JSON array)
//      /stream/dataset      DB cursor, raw record shape    streamed, no CL
//      /stream/dbobjects    DB cursor -> TPerson object    streamed, no CL
//      /stream/objectlist   TObjectList<TPerson>           streamed, no CL
//      /stream/enumeration  lazy file enumerator           streamed, no CL
//
//    TMVCSSEWriter        text/event-stream      (Server-Sent Events)
//      /stream/sse          one event per DB row           streamed, no CL
//
//    TMVCJSONLWriter      application/x-ndjson   (JSON Lines / NDJSON)
//      /stream/jsonl        one JSON object per line       streamed, no CL
//
//    TMVCCSVWriter        text/csv
//      /stream/csv          one CSV row per DB row         streamed, no CL
//
//    Functional / declarative (no explicit writer)
//      /stream/datasetfunc      return TDataSet             buffered, w/ CL
//      /stream/datasetstreamed  return TMVCStreamedResponse chunked, no CL
//
//  Server: Indy Direct, port 8991.  DB: SQLite (people.db), 200k rows.
//
//  NOTE: all streaming writers require an Indy-based backend (Indy Direct or
//  WebBroker on TIdHTTPWebBrokerBridge); they cannot stream over HTTP.sys.
// ===========================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  FireDAC.Phys.SQLite,
  FireDAC.Stan.Def,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Comp.Client,
  FireDAC.DApt,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.ActiveRecord,
  MVCFramework.Middleware.ActiveRecord,
  MVCFramework.SQLGenerators.Sqlite,
  StreamedArrayWriterConfigU in 'StreamedArrayWriterConfigU.pas',
  PeopleSourcesU in 'PeopleSourcesU.pas',
  StreamedArrayWriterControllerU in 'StreamedArrayWriterControllerU.pas';

{$R *.res}

const
  PORT = 8991;

procedure RunServer;
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  LEngine := TMVCEngine.Create;
  try
    LEngine.AddController(TStreamedArrayWriterController);
    // Provides the request-scoped connection used by the /datasetfunc action.
    LEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create(CON_DEF_NAME));

    LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
    LogI('Server listening on http://localhost:%d (Indy Direct)', [PORT]);
    LogI('GET /stream/dataset          - JSONArray  DB cursor (raw)     -> streamed, no CL');
    LogI('GET /stream/dbobjects        - JSONArray  DB cursor->TPerson  -> streamed, no CL');
    LogI('GET /stream/objectlist       - JSONArray  TObjectList<T>      -> streamed, no CL');
    LogI('GET /stream/enumeration      - JSONArray  lazy file enum      -> streamed, no CL');
    LogI('GET /stream/sse              - SSE        DB cursor -> events -> streamed, no CL');
    LogI('GET /stream/jsonl            - JSONL      DB cursor->ndjson   -> streamed, no CL');
    LogI('GET /stream/csv              - CSV        DB cursor->csv rows -> streamed, no CL');
    LogI('GET /stream/datasetfunc      - JSON       return TDataSet     -> buffered, w/ CL');
    LogI('GET /stream/datasetstreamed  - JSON       TMVCStreamedResp    -> chunked, no CL');
    LogI('CTRL+C to shutdown.');
    LServer.RunAndWait(PORT)
  finally
    LEngine.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    LogI('** DMVCFramework Streamed Array Writer Demo **');
    RegisterSQLiteConnectionDef;
    SeedDataset;
    SeedFileFeed;
    RunServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
