program ChatClient;

uses
  Forms,
  MainFormClient in 'MainFormClient.pas' {Form5},
  StompClient in '..\..\..\StompClient.pas',
  StompTypes in '..\..\..\StompTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
