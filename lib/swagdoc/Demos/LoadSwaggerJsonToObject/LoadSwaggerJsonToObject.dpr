program LoadSwaggerJsonToObject;

uses
  Vcl.Forms,
  frmLoadSwaggerJson in 'frmLoadSwaggerJson.pas' {frmSimpleSwaggerDocDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSimpleSwaggerDocDemo, frmSimpleSwaggerDocDemo);
  Application.Run;
end.
