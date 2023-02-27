program udp_syslog;

uses
  Vcl.Forms,
  UDPServerClientForm in 'UDPServerClientForm.pas' {FUDPServerClientForm},
  LoggerProConfig in 'LoggerProConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFUDPServerClientForm, FUDPServerClientForm);
  Application.Run;
end.
