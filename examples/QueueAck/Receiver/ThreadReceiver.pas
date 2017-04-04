unit ThreadReceiver;

interface

uses
  System.Classes,
  StompClient,
  StompTypes;

type
  TThreadReceiver = class(TThread)
  private
    FStompClient: TStompClient;
    FStompFrame: IStompFrame;
    procedure SetStompClient(const Value: TStompClient);
  protected
    procedure Execute; override;
  public
    procedure ReceiveAlarmMemo;
    procedure UpdateMessageMemo;
    procedure UpdateMessageIdEdit;
    constructor Create(CreateSuspended: Boolean); overload;
    property StompClient: TStompClient read FStompClient write SetStompClient;
  end;

implementation

{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

  Synchronize(UpdateCaption);

  and UpdateCaption could look like,

  procedure TThreadReceiver.UpdateCaption;
  begin
  Form1.Caption := 'Updated in a thread';
  end;

  or

  Synchronize(
  procedure
  begin
  Form1.Caption := 'Updated in thread via an anonymous method'
  end
  )
  );

  where an anonymous method is passed.

  Similarly, the developer can call the Queue method with similar parameters as
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.

}

uses ReceiverForm, System.SysUtils;

{ TThreadReceiver }

constructor TThreadReceiver.Create(CreateSuspended: Boolean);
begin
  FStompFrame := TStompFrame.Create;
  inherited Create(CreateSuspended);
end;

procedure TThreadReceiver.Execute;
begin
  NameThreadForDebugging('ThreadReceiver');

  while not Terminated do
  begin
    if FStompClient.Receive(FStompFrame, 2000) then
    begin
      Sleep(100);
      Synchronize(ReceiveAlarmMemo);
      Synchronize(UpdateMessageIdEdit);
    end
    else
    begin
      Synchronize(UpdateMessageMemo);
    end;
  end;
end;

procedure TThreadReceiver.ReceiveAlarmMemo;
begin
  ReceiverMainForm.MessageMemo.Lines.Add(
    StringReplace(FStompFrame.Output, #10, sLineBreak, [rfReplaceAll]));
end;

procedure TThreadReceiver.SetStompClient(const Value: TStompClient);
begin
  FStompClient := Value;
end;

procedure TThreadReceiver.UpdateMessageIdEdit;
begin
  ReceiverMainForm.MessageIdEdit.Text := FStompFrame.GetHeaders.Value('message-id');
end;

procedure TThreadReceiver.UpdateMessageMemo;
begin
  //ReceiverMainForm.MessageMemo.Lines.Add('Wait Messages....');
end;

end.
