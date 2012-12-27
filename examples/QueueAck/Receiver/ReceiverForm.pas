unit ReceiverForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, StompClient, StompTypes,
  ThreadReceiver;

type
  TReceiverMainForm = class(TForm)
    MessageMemo: TMemo;
    Label1: TLabel;
    SubscribeButton: TButton;
    QueueEdit: TEdit;
    Label2: TLabel;
    SendAckButton: TButton;
    SendNackButton: TButton;
    Label3: TLabel;
    UnsubscribeButton: TButton;
    MessageIdEdit: TEdit;
    Label4: TLabel;
    LogMemo: TMemo;
    Label5: TLabel;
    procedure SubscribeButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SendNackButtonClick(Sender: TObject);
    procedure SendAckButtonClick(Sender: TObject);
    procedure UnsubscribeButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    StompClient: TStompClient;
    StompFrame: IStompFrame;
    ThReceiver: TThreadReceiver;
    procedure BeforeSendFrame(AFrame: IStompFrame);
  public
  end;

var
  ReceiverMainForm: TReceiverMainForm;

implementation

{$R *.dfm}

procedure TReceiverMainForm.BeforeSendFrame(AFrame: IStompFrame);
begin
  LogMemo.Lines.Add(AFrame.Output);
end;

procedure TReceiverMainForm.FormCreate(Sender: TObject);
begin
  StompClient := TStompClient.Create;
  try
    StompClient.Connect;
  except
    on e: exception do
    begin
      raise Exception.Create('Cannot connect to Apollo server. Run the server and restart the application');
    end;
  end;
  StompClient.OnBeforeSendFrame := BeforeSendFrame;
  StompFrame := StompUtils.NewFrame();
  ThReceiver := TThreadReceiver.Create(True);
  ThReceiver.StompClient := StompClient;
end;

procedure TReceiverMainForm.FormDestroy(Sender: TObject);
begin
  ThReceiver.Free;
  StompClient.Free;
end;

procedure TReceiverMainForm.SendAckButtonClick(Sender: TObject);
begin
  if MessageIdEdit.Text='' then raise Exception.Create('Specify MessageId');

  if StompClient.Connected then
  begin
    StompClient.Ack(MessageIdEdit.Text);
  end;
end;

procedure TReceiverMainForm.SendNackButtonClick(Sender: TObject);
begin
  if MessageIdEdit.Text='' then raise Exception.Create('Specify MessageId');

  if StompClient.Connected then
  begin
    StompClient.Nack(MessageIdEdit.Text);
  end;
end;

procedure TReceiverMainForm.SubscribeButtonClick(Sender: TObject);
begin
  if QueueEdit.Text=''       then raise Exception.Create('Specify queue name');

  if StompClient.Connected then
  begin
     StompClient.Subscribe(QueueEdit.Text,amClientIndividual);
    //StompClient.Subscribe(QueueEdit.Text,amAuto);
    ThReceiver.Resume;
  end;
end;

procedure TReceiverMainForm.UnsubscribeButtonClick(Sender: TObject);
begin
  if QueueEdit.Text=''       then raise Exception.Create('Specify queue name');

  if StompClient.Connected then
  begin
    StompClient.Unsubscribe(QueueEdit.Text);
    ThReceiver.Suspend;
  end;
end;

end.
