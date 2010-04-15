unit MainForm;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, StompClient, StompTypes;

type
  TfrmMain = class(TForm)
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    chkPersistent: TCheckBox;
    Memo1: TMemo;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    stomp: TStompClient;
    tr: string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation


procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if InputQuery('Begin transaction','Write a transaction identifier for this transaction', tr) then
    Stomp.BeginTransaction(tr);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  if InputQuery('Abort transaction','Write a transaction identifier for this transaction', tr) then
  begin
    Stomp.AbortTransaction(tr);
    tr := '';
  end;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  if InputQuery('Commit transaction','Write a transaction identifier for this transaction', tr) then
  begin
    Stomp.CommitTransaction(tr);
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
  if tr<>'' then
    Stomp.Send(Edit1.Text, Memo1.Lines.Text, tr,h)
  else
    Stomp.Send(Edit1.Text, Memo1.Lines.Text,h);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  stomp.Disconnect;
  stomp.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  stomp := TStompClient.Create;
  stomp.Connect('localhost');
  tr := '';
end;

initialization
  {$i MainForm.lrs}

end.
