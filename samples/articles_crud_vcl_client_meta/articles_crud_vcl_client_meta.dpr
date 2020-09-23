program articles_crud_vcl_client_meta;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  MVCFramework.Serializer.Defaults in '..\..\sources\MVCFramework.Serializer.Defaults.pas',
  MVCFramework.FireDAC.Utils in '..\..\sources\MVCFramework.FireDAC.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
