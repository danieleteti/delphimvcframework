unit SendMessageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, StompClient, StompTypes,
  Vcl.ExtCtrls;

type
  TSendMessageMainForm = class(TForm)
    QueueMemo: TMemo;
    SendMessageButton: TButton;
    QueueEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LogMemo: TMemo;
    AutomaticSendTimer: TTimer;
    AutomaticSendCheckBox: TCheckBox;
    procedure SendMessageButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AutomaticSendTimerTimer(Sender: TObject);
    procedure AutomaticSendCheckBoxClick(Sender: TObject);
  private
    StompClient: TStompClient;
    procedure BeforeSendFrame(AFrame: IStompFrame);
    procedure Send;
  public
  end;

var
  SendMessageMainForm: TSendMessageMainForm;

implementation

{$R *.dfm}


procedure TSendMessageMainForm.AutomaticSendCheckBoxClick(Sender: TObject);
begin
  if QueueEdit.Text = '' then
    raise Exception.Create('Specify queue name');
  if QueueMemo.Lines.Text = '' then
    raise Exception.Create('Specify text of message to be sent');
  if AutomaticSendCheckBox.Checked then
    AutomaticSendTimer.Enabled := True
  else
    AutomaticSendTimer.Enabled := False;
end;

procedure TSendMessageMainForm.BeforeSendFrame(AFrame: IStompFrame);
begin
  LogMemo.Lines.Add(StringReplace(AFrame.Output, #10, sLineBreak, [rfReplaceAll]));
end;

procedure TSendMessageMainForm.FormCreate(Sender: TObject);
begin
  StompClient := TStompClient.Create;
  StompClient.OnBeforeSendFrame := BeforeSendFrame;
end;

procedure TSendMessageMainForm.FormDestroy(Sender: TObject);
begin
  StompClient.Free;
end;

procedure TSendMessageMainForm.AutomaticSendTimerTimer(Sender: TObject);
begin
  if AutomaticSendCheckBox.Checked then
  begin
    AutomaticSendTimer.Enabled := False;
    Send;
    AutomaticSendTimer.Enabled := True;
  end;
end;

procedure TSendMessageMainForm.Send;
begin
  StompClient.Connect;
  try
    try
      StompClient.Send(QueueEdit.Text, QueueMemo.Lines.Text);
    except
      on e: Exception do
      begin
        LogMemo.Lines.Add('ERROR: ' + e.Message);
      end;
    end;
  finally
    StompClient.Disconnect;
  end;
end;

procedure TSendMessageMainForm.SendMessageButtonClick(Sender: TObject);
begin
  if QueueEdit.Text = '' then
    raise Exception.Create('Specify queue name');
  if QueueMemo.Lines.Text = '' then
    raise Exception.Create('Specify text of message to be sent');
  Send;
end;

end.
