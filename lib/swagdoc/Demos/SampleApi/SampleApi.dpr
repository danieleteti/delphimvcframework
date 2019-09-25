program SampleApi;

uses
  System.StartUpCopy,
  FMX.Forms,
  Sample.Main in 'Sample.Main.pas' {Form1},
  Sample.Data.Employee in 'Sample.Data.Employee.pas',
  Sample.SwagDoc in 'Sample.SwagDoc.pas',
  Sample.Api.Employee in 'Sample.Api.Employee.pas',
  Sample.Data.Address in 'Sample.Data.Address.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
