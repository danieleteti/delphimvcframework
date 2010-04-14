unit MainFormClient;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Forms, StdCtrls, ExtCtrls, LResources, StompClient;

type

  TStompFrameReceiveEvent = procedure of object;

  { TReceiverThread }
  TReceiverThread = class(TThread)
  private
      FOnReceiveEvent: TStompFrameReceiveEvent;
  protected
      procedure Execute; override;
  public
      property OnReceiveEvent: TStompFrameReceiveEvent read FOnReceiveEvent write FOnReceiveEvent;
  end;


  { TForm5 }

  TForm5 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    tmr: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Receive;
    procedure Button2Click(Sender: TObject);
    procedure Memo2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    stomp: TStompClient;
    roomname: string;
    receiver: TReceiverThread;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  StompTypes;

{ TReceiverThread }
procedure TReceiverThread.Execute;
begin
     while not Terminated do
     begin
          if Assigned(OnReceiveEvent)
          then
              OnReceiveEvent;
     end;
end;

{ TForm5 }
procedure TForm5.Button1Click(Sender: TObject);
begin
  roomname := '/topic/' + Edit2.Text;
  stomp := TStompClient.Create;
  stomp.Connect(Edit1.Text);

  //Setup for reading messages
  stomp.Subscribe(roomname, amClient);

  Button1.Enabled := False;
  Edit1.Enabled := False;
  Edit2.Enabled := False;
  Edit3.Enabled := False;
  Button2.Enabled := True;
  Memo2.Enabled := True;

  receiver.Resume;
end;

procedure TForm5.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     receiver.FreeOnTerminate:=true;
     receiver.Terminate;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
     receiver:=TReceiverThread.Create(true);
     receiver.OnReceiveEvent:=Receive;
end;

procedure TForm5.Button2Click(Sender: TObject);
var
   h: IStompHeaders;
begin
  h:=StompUtils.NewHeaders;
  h.Add('sender', Edit3.Text);
  h.Add('datetime', formatdatetime('yyyy/mm/dd hh:nn:ss', now));
  h.Add(TStompHeaders.newPersistentHeader(true));

  stomp.Send(roomname, Memo2.Lines.Text, h);

  Memo2.Lines.Clear;
end;

procedure TForm5.Memo2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 13) and not (ssCtrl in Shift) then
  begin
    key := 0;
    Button2.Click;
  end;
end;

procedure TForm5.tmrTimer(Sender: TObject);
begin
     Application.ProcessMessages;
end;

procedure TForm5.Receive;
var
  f: IStompFrame;
begin
  f := stomp.Receive;
  if assigned(f) then
  begin
       Memo1.Lines.Add('[' + f.GetHeaders.Value('datetime') + ' ' + f.GetHeaders.Value('sender') + ']' + sLineBreak + f.GetBody);
  end;
end;

initialization
  {$i MainFormClient.lrs}

end.
