program WineCellarClientRESTAdapter;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFMX in 'MainFMX.pas' {TabbedForm} ,
  RESTServicesU in 'RESTServicesU.pas',
  WinesBO in '..\winecellar\WinesBO.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TTabbedForm, TabbedForm);
  Application.Run;

end.
