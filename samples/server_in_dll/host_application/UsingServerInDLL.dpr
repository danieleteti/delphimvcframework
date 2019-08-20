program UsingServerInDLL;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  REST.RestServer in '..\REST\REST.RestServer.pas',
  REST.WebModule in '..\REST\REST.WebModule.pas' {MainWebModule: TWebModule},
  REST.MainController in '..\REST\CONTROLLERS\REST.MainController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
