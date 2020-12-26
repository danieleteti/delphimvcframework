library DMVCServerDLL;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  REST.RestServer in '..\REST\REST.RestServer.pas',
  REST.WebModule in '..\REST\REST.WebModule.pas' {MainWebModule: TWebModule},
  REST.MainController in '..\REST\CONTROLLERS\REST.MainController.pas';

{$R *.res}


var
  gRESTServer: TDMVCRestServer;

procedure RunServer(const Port: Integer); stdcall;
begin
  gRESTServer := TDMVCRestServer.Create(Port);
  gRESTServer.Activate;
end;

procedure StopServer; stdcall;
begin
  FreeAndNil(gRESTServer);
end;

exports
  RunServer,
  StopServer;

begin

end.
