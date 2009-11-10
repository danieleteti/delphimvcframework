program Receiver;

uses
  Forms,
  ReceiverForm in 'ReceiverForm.pas' {Form1},
  StompClient in '..\..\..\StompClient.pas',
  StompTypes in '..\..\..\StompTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
