unit ReceiverForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  StompClient,
  StompTypes,
  ExtCtrls;

type
  TForm1 = class(TForm, IStompClientListener)
    Edit1: TEdit;
    Button1: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    pnlConnection: TPanel;
    Button2: TButton;
    edtUserName: TLabeledEdit;
    edtPassword: TLabeledEdit;
    edtHostNameAndPort: TLabeledEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    stomp: IStompClient;
    th: TStompClientListener;

  public
    procedure OnMessage(StompClient: IStompClient; StompFrame: IStompFrame; var StompListening: Boolean);
    procedure OnStopListen(StompClient: IStompClient);

  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  stomp.Subscribe(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: string;
  hostname: string;
  port: string;
begin
  s := edtHostNameAndPort.Text;
  hostname := Copy(s, 1, Pos(':', s) - 1);
  port := Copy(s, Pos(':', s) + 1, length(s));
  stomp.SetUserName(edtUserName.Text);
  stomp.SetPassword(edtPassword.Text);
  stomp.Connect('localhost', 61613, 'myclientid', TStompAcceptProtocol.STOMP_Version_1_0);
  th := TStompClientListener.Create(stomp, Self);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  stomp.Unsubscribe(Edit1.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  th.StopListening;
  FreeAndNil(th);
  stomp.Disconnect;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  stomp.Subscribe(Edit1.Text, amAuto, StompUtils.NewHeaders.Add(TStompHeaders.NewDurableSubscriptionHeader('pippo')));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(stomp) then
  begin
    if assigned(th) then
    begin
      th.StopListening;
      th.Free;
    end;
    stomp.Disconnect;
    stomp := nil;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  stomp := TStompClient.Create;
end;

procedure TForm1.OnMessage(StompClient: IStompClient; StompFrame: IStompFrame; var StompListening: Boolean);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Caption := 'Last message: ' + datetimetostr(now);
      Memo1.Lines.Add(StompFrame.GetBody);
    end);
end;

procedure TForm1.OnStopListen(StompClient: IStompClient);
begin
  ShowMessage('STOP LISTEN');
end;

end.
