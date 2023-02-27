program GenerateUnitFileForMVCFramework;

uses
  System.StartUpCopy,
  FMX.Forms,
  Sample.Main in 'Sample.Main.pas' {Form1},
  Sample.DelphiUnit.Generate in 'Sample.DelphiUnit.Generate.pas',
  Sample.MvcControllerClientFileBuilder in 'Sample.MvcControllerClientFileBuilder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
