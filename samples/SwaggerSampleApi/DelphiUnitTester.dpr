program DelphiUnitTester;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  DelphiUnit in 'DelphiUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
