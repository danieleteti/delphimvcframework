program Receiver;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, LResources, Interfaces,
  ReceiverForm in 'ReceiverForm.pas' {Form1},
  StompClient in '../../../StompClient.pas',
  StompTypes in '../../../StompTypes.pas';

{$IFDEF WINDOWS}{$R Receiver.rc}{$ENDIF}

begin
  {$I Receiver.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
