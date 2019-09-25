program SampleApi;

uses
  System.StartUpCopy,
  FMX.Forms,
  Sample.Main in 'Sample.Main.pas' {Form1},
  mvccontroller in 'mvccontroller.pas',
  Sample.SwagDoc.DelphiMVCFramework in 'Sample.SwagDoc.DelphiMVCFramework.pas',
  DelphiUnit in 'DelphiUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
