unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  StompTypes;

type
  TForm4 = class(TForm, IStompClientListener)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    th0: IStompListener;
    th1: IStompListener;
    th2: IStompListener;
    { Private declarations }
  public
    procedure OnMessage(StompClient: IStompClient; StompFrame: IStompFrame;
      var StopListening: boolean);
    procedure OnStopListen(StompClient: IStompClient);
  end;

var
  Form4: TForm4;

implementation

uses StompClient;

type
  TMyStompListener = class(TInterfacedObject, IStompClientListener)
  public
    procedure OnMessage(StompClient: IStompClient; StompFrame: IStompFrame;
      var StopListening: boolean);
    procedure OnStopListen(StompClient: IStompClient);
  end;

{$R *.dfm}


procedure TForm4.Button1Click(Sender: TObject);
var
  stomp0: IStompClient;
  stomp1: IStompClient;
  stomp2: IStompClient;
begin
  stomp0 := TStompClient.CreateAndConnect;
  stomp0.Subscribe('/topic/danieleteti', amAuto, StompUtils.NewHeaders.Add('include-seq', 'seq'));
  th0 := TStompClientListener.Create(stomp0, TMyStompListener.Create);

  stomp1 := TStompClient.CreateAndConnect;
  stomp1.Subscribe('/topic/danieleteti');
  th1 := TStompClientListener.Create(stomp1, self);

  stomp2 := TStompClient.CreateAndConnect;
  stomp2.Subscribe('/topic/salvatore');
  th2 := TStompClientListener.Create(stomp2, self);

  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      stomp: IStompClient;
    begin
      stomp := TStompClient.CreateAndConnect;
      for i := 1 to 10 do
      begin
        sleep(100);
        stomp.Send('/topic/danieleteti', 'Hello World ' + IntToStr(i));
        stomp.Send('/topic/salvatore', 'Hello World ' + IntToStr(i));
      end;
      stomp.Send('/topic/danieleteti', 'SHUTDOWN');
      stomp.Send('/topic/johndoe', 'SHUTDOWN');
      stomp.Disconnect;
    end).Start;
end;

procedure TForm4.OnMessage(StompClient: IStompClient; StompFrame: IStompFrame;
var StopListening: boolean);
begin
  if StompFrame.GetBody = 'SHUTDOWN' then
    StopListening := true;

  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(StompFrame.GetBody);
    end);
end;

procedure TForm4.OnStopListen(StompClient: IStompClient);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(StompClient.GetSession + ' has been stopped');
    end);
end;

{ TMyStompListener }

procedure TMyStompListener.OnMessage(StompClient: IStompClient; StompFrame: IStompFrame;
var StopListening: boolean);
begin
  if StompFrame.GetBody = 'SHUTDOWN' then
    StopListening := true;
  Writeln('------');
  Writeln(StompFrame.Output);
  Writeln('------');
end;

procedure TMyStompListener.OnStopListen(StompClient: IStompClient);
begin
  Writeln('Listener has been stopped');

end;

initialization

AllocConsole;

end.
