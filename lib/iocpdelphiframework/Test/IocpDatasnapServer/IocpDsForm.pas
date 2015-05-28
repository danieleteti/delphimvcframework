unit IocpDsForm;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, Web.HTTPApp, Vcl.ExtCtrls;

type
  TfmEsGUI = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    ButtonOpenBrowser: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
  public
  end;

var
  fmEsGUI: TfmEsGUI;

implementation

{$R *.dfm}

uses
  Winapi.ShellApi, Datasnap.DSSession, IocpDsServer;

procedure TfmEsGUI.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not TDataSnapServer.Active;
  ButtonStop.Enabled := TDataSnapServer.Active;
  EditPort.Enabled := not TDataSnapServer.Active;
end;

procedure TfmEsGUI.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  ButtonStartClick(nil);
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TfmEsGUI.ButtonStartClick(Sender: TObject);
begin
  TDataSnapServer.StartServer(StrToInt(EditPort.Text));
end;

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

procedure TfmEsGUI.ButtonStopClick(Sender: TObject);
begin
  TerminateThreads;
  TDataSnapServer.StopServer;
end;

end.
