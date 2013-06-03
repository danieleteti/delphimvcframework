unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StompClient, StompTypes, Vcl.ExtCtrls, System.Actions,
  Vcl.ActnList;

type
  TfrmMain = class(TForm)
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    chkPersistent: TCheckBox;
    Memo1: TMemo;
    Button6: TButton;
    Button1: TButton;
    edtUserName: TLabeledEdit;
    edtPassword: TLabeledEdit;
    pnlConnection: TPanel;
    pnlMain: TPanel;
    edtHostNameAndPort: TLabeledEdit;
    ActionList1: TActionList;
    acConnect: TAction;
    Button5: TButton;
    acDisconnect: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acDisconnectExecute(Sender: TObject);
  private
    stomp: IStompClient;
    tr: string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.acConnectExecute(Sender: TObject);
var
  s: string;
  hostname: string;
  port: string;
begin
  s := edtHostNameAndPort.Text;
  hostname := Copy(s, 1, Pos(':', s) - 1);
  port := Copy(s, Pos(':', s) + 1, length(s));
  stomp.SetUserName(edtUserName.Text).SetPassword(edtPassword.Text).Connect(hostname, strtoint(port),
    inttostr(Random(100000000)), TStompAcceptProtocol.STOMP_Version_1_1);

end;

procedure TfrmMain.acDisconnectExecute(Sender: TObject);
begin
  stomp.Disconnect;
end;

procedure TfrmMain.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acConnect.Enabled := not stomp.Connected;
  acDisconnect.Enabled := stomp.Connected;
  pnlMain.Enabled := stomp.Connected;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if InputQuery('Begin transaction', 'Write a transaction identifier for this transaction', tr) then
    stomp.BeginTransaction(tr);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  if InputQuery('Abort transaction', 'Write a transaction identifier for this transaction', tr) then
  begin
    stomp.AbortTransaction(tr);
    tr := '';
  end;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  if InputQuery('Commit transaction', 'Write a transaction identifier for this transaction', tr) then
  begin
    stomp.CommitTransaction(tr);
    tr := '';
  end;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
var
  h: IStompHeaders;
begin
  h := StompUtils.NewHeaders;
  if chkPersistent.Checked then
    h.Add(TStompHeaders.NewPersistentHeader(true));
  if tr <> '' then
    stomp.Send(Edit1.Text, Memo1.Lines.Text, tr, h)
  else
    stomp.Send(Edit1.Text, Memo1.Lines.Text, h);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  stomp.Disconnect;
  stomp := nil;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  stomp := TStompClient.Create;
  // stomp.Connect('localhost');
  tr := '';
end;

end.
