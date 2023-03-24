program vcl_appenders;

uses
  Vcl.Forms,
  VCLAppendersFormU in 'VCLAppendersFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
