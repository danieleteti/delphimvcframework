program avoid_mid_air_collisions_sample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Cache,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  MVCFramework.SQLGenerators.PostgreSQL,
  IdHTTPWebBrokerBridge,
  MainControllerU in 'MainControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  MVCFramework.Utils in '..\..\sources\MVCFramework.Utils.pas',
  Entities in '..\activerecord_restful_crud\Entities.pas',
  MVCFramework.ActiveRecord;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;

    { more info about MaxConnections
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.ListenQueue := 200;

    LServer.Active := True;
    WriteLn('Listening on port ', APort);
    WriteLn('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

procedure LoadFakeData;
begin
  ActiveRecordConnectionsRegistry.AddDefaultConnection('activerecorddb');
  try
    TMVCActiveRecord.DeleteAll(TPerson);

    with TPerson.Create do
    try
      FirstName := 'Daniele';
      LastName := 'Teti';
      DOB := nil;
      IsMale := True;
      Insert;
    finally
      Free;
    end;

    with TPerson.Create do
    try
      FirstName := 'Peter';
      LastName := 'Parker';
      DOB := EncodeDate(1960,10,11);
      IsMale := True;
      Insert;
    finally
      Free;
    end;

    with TPerson.Create do
    try
      FirstName := 'Sue';
      LastName := 'Storm';
      DOB := EncodeDate(1970,2,11);
      IsMale := False;
      Insert;
    finally
      Free;
    end;

  finally
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection();
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    LoadFakeData;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

