program serversentevents2viewer;

uses
  Vcl.Forms,
  MainViewerFormU in 'MainViewerFormU.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
