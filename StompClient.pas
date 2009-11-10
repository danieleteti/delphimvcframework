{ ******************************************************* }
{                                                         }
{ Stomp Client for Embarcadero Delphi                     }
{ Tested With ApacheMQ 5.2/5.3                            }
{ Copyright (c) 2009-2009 Daniele Teti                    }
{                                                         }
{                                                         }
{ WebSite: www.danieleteti.it                             }
{ email:d.teti@bittime.it                                 }
{ ******************************************************* }

unit StompClient;

interface

uses
  StompTypes,
  IdTCPClient,
  SysUtils,
  IdException,
  IdExceptionCore,
  Classes;

type
  TStompClient = class(TInterfacedObject, IStompClient)
  private
    FTCP: TIdTCPClient;
    FHeaders: IStompHeaders;
    FPassword: string;
    FUserName: string;
    FTimeout: Integer;
    FSession: string;
    FInTransaction: Boolean;
    FTransactions: TStringList;
    FReceiptTimeout: Integer;
    procedure SetReceiptTimeout(const Value: Integer);

  protected
    procedure Init;
    procedure DeInit;
    procedure MergeHeaders(var AFrame: IStompFrame; var AHeaders: IStompHeaders);
    procedure SendFrame(AFrame: IStompFrame);

  public
    function SetPassword(const Value: string): IStompClient;
    function SetUserName(const Value: string): IStompClient;
    function Receive(out StompFrame: IStompFrame; ATimeout: Integer): Boolean; overload;
    function Receive: IStompFrame; overload;
    function Receive(ATimeout: Integer): IStompFrame; overload;
    procedure Receipt(const ReceiptID: string);
    procedure Connect(Host: string = '127.0.0.1'; Port: Integer = DEFAULT_STOMP_PORT; ClientID: string = '');
    procedure Disconnect;
    procedure Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto; Headers: IStompHeaders = nil);
    procedure Unsubscribe(Queue: string);
    procedure Send(QueueOrTopicName: string; TextMessage: string; Headers: IStompHeaders = nil); overload;
    procedure Send(QueueOrTopicName: string; TextMessage: string; TransactionIdentifier: string;
      Headers: IStompHeaders = nil); overload;
    procedure Ack(const MessageID: string; const TransactionIdentifier: string = '');
    procedure BeginTransaction(const TransactionIdentifier: string);
    procedure CommitTransaction(const TransactionIdentifier: string);
    procedure AbortTransaction(const TransactionIdentifier: string);
    /// ////////////
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;
    function SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
    property Session: string read FSession;
    property ReceiptTimeout: Integer read FReceiptTimeout write SetReceiptTimeout;
    property Transactions: TStringList read FTransactions;
  end;

implementation

uses
  Windows,
  IdGlobal,
  Character;

{ TStompClient }

procedure TStompClient.AbortTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) > -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.SetCommand('ABORT');
    Frame.GetHeaders.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    FInTransaction := False;
    FTransactions.Delete(FTransactions.IndexOf(TransactionIdentifier));
  end
  else
    raise EStomp.CreateFmt('Abort Transaction Error. Transaction [%s] not found', [TransactionIdentifier]);
end;

procedure TStompClient.Ack(const MessageID: string; const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('ACK');
  Frame.GetHeaders.Add('message-id', MessageID);
  if TransactionIdentifier <> '' then
    Frame.GetHeaders.Add('transaction', TransactionIdentifier);
  SendFrame(Frame);
end;

procedure TStompClient.BeginTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) = -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.SetCommand('BEGIN');
    Frame.GetHeaders.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    // CheckReceipt(Frame);
    FInTransaction := True;
    FTransactions.Add(TransactionIdentifier);
  end
  else
    raise EStomp.CreateFmt('Begin Transaction Error. Transaction [%s] still open', [TransactionIdentifier]);
end;

// procedure TStompClient.CheckReceipt(Frame: TStompFrame);
// var
// ReceiptID: string;
// begin
// if FEnableReceipts then
// begin
// ReceiptID := inttostr(GetTickCount);
// Frame.GetHeaders.Add('receipt', ReceiptID);
// SendFrame(Frame);
// Receipt(ReceiptID);
// end
// else
// SendFrame(Frame);
// end;

procedure TStompClient.CommitTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) > -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.SetCommand('COMMIT');
    Frame.GetHeaders.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    FInTransaction := False;
    FTransactions.Delete(FTransactions.IndexOf(TransactionIdentifier));
  end
  else
    raise EStomp.CreateFmt('Commit Transaction Error. Transaction [%s] not found', [TransactionIdentifier]);
end;

procedure TStompClient.Connect(Host: string; Port: Integer; ClientID: string);
var
  Frame: IStompFrame;
begin
  try
    Init;
    FTCP.Connect(Host, Port);
    FTCP.IOHandler.MaxLineLength := MaxInt;
    Frame := TStompFrame.Create;
    Frame.SetCommand('CONNECT');
    Frame.GetHeaders.Add('login', FUserName).Add('passcode', FPassword);
    if ClientID <> '' then
      Frame.GetHeaders.Add('client-id', ClientID);
    SendFrame(Frame);
    Frame := nil;
    while Frame = nil do
      Frame := Receive;
    if Frame.GetCommand = 'ERROR' then
      raise EStomp.Create(Frame.output);
    if Frame.GetCommand = 'CONNECTED' then
    begin
      FSession := Frame.GetHeaders.Value('session');
    end;
    { todo: 'Call event?' }
  except
    on E: Exception do
    begin
      raise EStomp.Create(E.Message);
    end;
  end;
end;

function TStompClient.Connected: Boolean;
begin
  Result := Assigned(FTCP) and FTCP.Connected;
end;

constructor TStompClient.Create;
begin
  inherited;
  FInTransaction := False;
  FSession := '';
  FUserName := 'guest';
  FPassword := 'guest';
  FHeaders := TStompHeaders.Create;
  FTimeout := 200;
  FReceiptTimeout := FTimeout;
end;

procedure TStompClient.DeInit;
begin
  FreeAndNil(FTCP);
  FreeAndNil(FTransactions);
end;

destructor TStompClient.Destroy;
begin
  DeInit;
  inherited;
end;

procedure TStompClient.Disconnect;
var
  Frame: IStompFrame;
begin
  if Connected then
  begin
    Frame := TStompFrame.Create;
    Frame.SetCommand('DISCONNECT');
    SendFrame(Frame);
    FTCP.Disconnect;
  end;
  DeInit;
end;

procedure TStompClient.Init;
begin
  DeInit;
  FTCP := TIdTCPClient.Create(nil);
  FTransactions := TStringList.Create;
end;

procedure TStompClient.MergeHeaders(var AFrame: IStompFrame; var AHeaders: IStompHeaders);
var
  i: Integer;
  h: TKeyValue;
begin
  if Assigned(AHeaders) then
    if AHeaders.Count > 0 then
      for i := 0 to AHeaders.Count - 1 do
      begin
        h := AHeaders.GetAt(i);
        AFrame.GetHeaders.Add(h.Key, h.Value);
      end;
end;

procedure TStompClient.Receipt(const ReceiptID: string);
var
  Frame: IStompFrame;
begin
  if Receive(Frame, FReceiptTimeout) then
  begin
    if Frame.GetCommand <> 'RECEIPT' then
      raise EStomp.Create('Receipt command error');
    if Frame.GetHeaders.Value('receipt-id') <> ReceiptID then
      raise EStomp.Create('Receipt receipt-id error');
  end;
end;

function TStompClient.Receive(out StompFrame: IStompFrame; ATimeout: Integer): Boolean;
begin
  StompFrame := nil;
  StompFrame := Receive(ATimeout);
  Result := Assigned(StompFrame);
end;

function TStompClient.Receive(ATimeout: Integer): IStompFrame;
var
  s: string;
  c: Char;
  sb: TStringBuilder;
  tout: Boolean;
begin
  tout := False;
  Result := nil;
  try
    sb := TStringBuilder.Create(1024);
    try
      FTCP.ReadTimeout := ATimeout;
      try
        FTCP.IOHandler.CheckForDataOnSource(1);
        while True do
        begin
          c := FTCP.IOHandler.ReadChar;
          if c <> CHAR0 then
            sb.Append(c)
          else
          begin
            FTCP.IOHandler.ReadChar;
            Break;
          end;
        end;
      except
        on E: EIdReadTimeout do
        begin
          tout := True;
        end;
        on E: Exception do
        begin
          raise ;
        end;
      end;
      if not tout then
        Result := StompUtils.CreateFrame(sb.ToString + #0);
    finally
      sb.Free;
    end;
  except
    on E: Exception do
    begin
      raise ;
    end;
  end;
end;

function TStompClient.Receive: IStompFrame;
begin
  Result := Receive(FTimeout);
end;

procedure TStompClient.Send(QueueOrTopicName: string; TextMessage: string; Headers: IStompHeaders);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('SEND');
  Frame.GetHeaders.Add('destination', QueueOrTopicName);
  Frame.SetBody(TextMessage);
  MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;

procedure TStompClient.Send(QueueOrTopicName: string; TextMessage: string; TransactionIdentifier: string;
  Headers: IStompHeaders);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('SEND');
  Frame.GetHeaders.Add('destination', QueueOrTopicName);
  Frame.GetHeaders.Add('transaction', TransactionIdentifier);
  Frame.SetBody(TextMessage);
  MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;

procedure TStompClient.SendFrame(AFrame: IStompFrame);
begin
  FTCP.IOHandler.Write(TEncoding.ASCII.GetBytes(AFrame.output));
end;

function TStompClient.SetPassword(const Value: string): IStompClient;
begin
  FPassword := Value;
  Result := Self;
end;

procedure TStompClient.SetReceiptTimeout(const Value: Integer);
begin
  FReceiptTimeout := Value;
end;

function TStompClient.SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
begin
  FTimeout := AMilliSeconds;
  Result := Self;
end;

function TStompClient.SetUserName(const Value: string): IStompClient;
begin
  FUserName := Value;
  Result := Self;
end;

procedure TStompClient.Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto; Headers: IStompHeaders = nil);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('SUBSCRIBE');
  Frame.GetHeaders.Add('destination', QueueOrTopicName).Add('ack', StompUtils.AckModeToStr(Ack));
  if Headers <> nil then
    MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;

procedure TStompClient.Unsubscribe(Queue: string);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('UNSUBSCRIBE');
  Frame.GetHeaders.Add('destination', Queue);
  SendFrame(Frame);
end;

end.
