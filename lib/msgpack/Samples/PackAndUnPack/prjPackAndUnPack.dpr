program prjPackAndUnPack;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form2},
  SimpleMsgPack in '..\..\SimpleMsgPack.pas',
  uByteTools in 'uByteTools.pas',
  DMsgPackHelper in '..\..\DMsgPackHelper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;   
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
