program SendMessage;

uses
  Vcl.Forms,
  SendMessageForm in 'SendMessageForm.pas' {SendMessageMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSendMessageMainForm, SendMessageMainForm);
  Application.Run;
end.
