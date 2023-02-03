program LuaPlayground;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  LuaBind.Intf in '..\..\LuaBind.Intf.pas',
  LuaBind in '..\..\LuaBind.pas',
  LuaBind.CustomType.DataSet in '..\..\LuaBind.CustomType.DataSet.pas',
  LuaBind.DelphiObjects in '..\..\LuaBind.DelphiObjects.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
