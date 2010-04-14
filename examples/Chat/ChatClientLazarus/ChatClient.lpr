program ChatClient;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, LResources, Interfaces,
  MainFormClient in 'MainFormClient.pas' {Form5},
  StompClient in '../../../StompClient.pas',
  StompTypes in '../../../StompTypes.pas';

{$IFDEF WINDOWS}{$R ChatClient.rc}{$ENDIF}

begin
  {$I ChatClient.lrs}
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
