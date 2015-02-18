program articles_crud_vcl_client;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
