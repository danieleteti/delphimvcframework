program Sample;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {Form5},
  EntitiesU in 'EntitiesU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
