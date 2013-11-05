program LuaTest;

uses
  Vcl.Forms,
  Unit11 in 'Unit11.pas' {Form11} ,
  LuaBind.Intf in '..\..\LuaBind.Intf.pas',
  LuaBind in '..\..\LuaBind.pas',
  LuaBind.CustomType.DataSet in '..\..\LuaBind.CustomType.DataSet.pas',
  LuaBind.DelphiObjects in '..\..\LuaBind.DelphiObjects.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;

end.
