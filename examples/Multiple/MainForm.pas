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
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FSTOMPListener: IStompListener;
    FSTOMPClient: IStompClient;
    FFormClosing: Boolean;
    FProducerThread: TThread;
  public
    procedure OnMessage(StompFrame: IStompFrame; var TerminateListener: Boolean);
    procedure OnListenerStopped(StompClient: IStompClient);
  end;

var
  Form4: TForm4;

implementation

uses StompClient;

{$R *.dfm}


procedure TForm4.Button1Click(Sender: TObject);
begin
  FSTOMPListener.StopListening;
  Memo1.Lines.Add('Listener Started');
  FSTOMPListener.StartListening;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  FSTOMPListener.StopListening;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  FProducerThread := TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      stomp: IStompClient;
    begin
      stomp := TStompClient.CreateAndConnect;
      i := 1;
      while True do
      begin
        sleep(300);
        if FFormClosing then
          Exit;
        stomp.Send('/topic/danieleteti', 'Hello World ' + IntToStr(i));
        inc(i);
      end;
      stomp.Disconnect;
    end);
  FProducerThread.FreeOnTerminate := False;
  FProducerThread.Start;
  Button3.Enabled := False;
  ShowMessage('Background thread started... Now you can start the subscriber');
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FFormClosing := False;
  FSTOMPClient := TStompClient.CreateAndConnect;
  FSTOMPClient.Subscribe('/topic/danieleteti',
    amAuto,
    StompUtils.Headers.Add('include-seq', 'seq'));
  FSTOMPListener := TStompClientListener.Create(FSTOMPClient, Self);
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FFormClosing := True;
  if Assigned(FProducerThread) then
  begin
    FProducerThread.WaitFor;
    FProducerThread.Free;
  end;
  FSTOMPListener := nil;
end;

procedure TForm4.OnMessage(StompFrame: IStompFrame; var TerminateListener: Boolean);
begin
  Memo1.Lines.Add(StompFrame.Body);
  TerminateListener := FFormClosing;
end;

procedure TForm4.OnListenerStopped(StompClient: IStompClient);
begin
  Memo1.Lines.Add('Listener Stopped');
end;

end.
