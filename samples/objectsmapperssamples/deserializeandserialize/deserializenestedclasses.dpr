program deserializenestedclasses;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {Form1},
  BusinessObjectsU in '..\mapperprimer\BusinessObjectsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
