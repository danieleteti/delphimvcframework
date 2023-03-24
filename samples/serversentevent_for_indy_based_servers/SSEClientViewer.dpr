program SSEClientViewer;

uses
  Vcl.Forms,
  MainSSEClientViewerFormU in 'MainSSEClientViewerFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
