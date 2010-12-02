unit ReceiverForm;

{$MODE Delphi}

interface

uses
  {Windows,}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  StompClient,
  StompTypes,
  ExtCtrls, LResources;

type
  TForm1 = class(TForm, IStompClientListener)
    Edit1: TEdit;
    Button1: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    stomp: IStompClient;
    th: TStompClientListener;

  public
    procedure OnMessage(StompFrame: IStompFrame);
  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  stomp.Subscribe(Edit1.Text);
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

procedure TForm1.Button6Click(Sender: TObject);
begin
  stomp.Connect('localhost', 61613, 'myclientid');
  th := TStompClientListener.Create(stomp, Self);
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

procedure TForm1.OnMessage(StompFrame: IStompFrame);
begin
  Caption := DateTimeToStr(StompUtils.TimestampAsDateTime(StompFrame.GetHeaders.Value('timestamp')));
  Memo1.Lines.Add(StompFrame.GetBody);
end;

initialization
  {$i ReceiverForm.lrs}

end.
