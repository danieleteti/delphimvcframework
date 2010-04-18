{ ******************************************************* }
{ }
{ Stomp Client for Embarcadero Delphi & FreePascal }
{ Tested With ApacheMQ 5.2/5.3 }
{ Copyright (c) 2009-2009 Daniele Teti }
{ }
{ Contributors: }
{ Daniel Gaspary: dgaspary@gmail.com }
{ }
{ WebSite: www.danieleteti.it }
{ email:d.teti@bittime.it }
{ ******************************************************* }

unit StompClient;

// For FreePascal users:
// Automatically selected synapse tcp library
{$IFDEF FPC}
{$MODE DELPHI}
{$DEFINE USESYNAPSE}
{$ENDIF}
// For Delphi users:
// Decomment following line to use synapse also in Delphi
{ .$DEFINE USESYNAPSE }

interface

uses
  StompTypes,
  SysUtils,
{$IFNDEF USESYNAPSE}
  IdTCPClient,
  IdException,
  IdExceptionCore,
{$ELSE}
  synsock,
  blcksock,
{$ENDIF}
  Classes;

type
{$IFDEF USESYNAPSE}
  ESynapseTimeout = Exception;
{$ENDIF}

  TStompClient = class(TInterfacedObject, IStompClient)
  private
{$IFDEF USESYNAPSE}
    FSynapseTCP: TTCPBlockSocket;
    FSynapseConnected: boolean;
{$ELSE}
    FTCP: TIdTCPClient;
{$ENDIF}
    FHeaders: IStompHeaders;
    FPassword: string;
    FUserName: string;
    FTimeout: Integer;
    FSession: string;
    FInTransaction: boolean;
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
    function Receive(out StompFrame: IStompFrame; ATimeout: Integer): boolean; overload;
    function Receive: IStompFrame; overload;
    function Receive(ATimeout: Integer): IStompFrame; overload;
    procedure Receipt(const ReceiptID: string);
    procedure Connect(Host: string = '127.0.0.1'; Port: Integer = DEFAULT_STOMP_PORT;
      ClientID: string = '');
    procedure Disconnect;
    procedure Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto;
      Headers: IStompHeaders = nil);
    procedure Unsubscribe(Queue: string);
    procedure Send(QueueOrTopicName: string; TextMessage: string; Headers: IStompHeaders = nil);
      overload;
    procedure Send(QueueOrTopicName: string; TextMessage: string; TransactionIdentifier: string;
      Headers: IStompHeaders = nil); overload;
    procedure Ack(const MessageID: string; const TransactionIdentifier: string = '');
    procedure BeginTransaction(const TransactionIdentifier: string);
    procedure CommitTransaction(const TransactionIdentifier: string);
    procedure AbortTransaction(const TransactionIdentifier: string);
    /// ////////////
    constructor Create; virtual;
    destructor Destroy; override;
    function Connected: boolean;
    function SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
    property Session: string read FSession;
    property ReceiptTimeout: Integer read FReceiptTimeout write SetReceiptTimeout;
    property Transactions: TStringList read FTransactions;
  end;

implementation

{$IFDEF FPC}

const
  CHAR0 = #0;
{$ELSE}

uses
  Windows,
  IdGlobal,
  Character;
{$ENDIF}
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
    raise EStomp.CreateFmt('Abort Transaction Error. Transaction [%s] not found',
      [TransactionIdentifier]);
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
    raise EStomp.CreateFmt('Begin Transaction Error. Transaction [%s] still open',
      [TransactionIdentifier]);
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
    raise EStomp.CreateFmt('Commit Transaction Error. Transaction [%s] not found',
      [TransactionIdentifier]);
end;

procedure TStompClient.Connect(Host: string; Port: Integer; ClientID: string);
var
  Frame: IStompFrame;
begin
  try
    Init;
{$IFDEF USESYNAPSE}
    FSynapseTCP.Connect(Host, intToStr(Port));
    FSynapseConnected := True;
{$ELSE}
    FTCP.Connect(Host, Port);
    FTCP.IOHandler.MaxLineLength := MaxInt;
{$ENDIF}
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
      raise EStomp.Create(E.message);
    end;
  end;
end;

function TStompClient.Connected: boolean;
begin
{$IFDEF USESYNAPSE}
  Result := Assigned(FSynapseTCP) and FSynapseConnected;
{$ELSE}
  Result := Assigned(FTCP) and FTCP.Connected;
{$ENDIF}
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
{$IFDEF USESYNAPSE}
  FreeAndNil(FSynapseTCP);
{$ELSE}
  FreeAndNil(FTCP);
{$ENDIF}
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
{$IFDEF USESYNAPSE}
    FSynapseTCP.CloseSocket;
    FSynapseConnected := False;
{$ELSE}
    FTCP.Disconnect;
{$ENDIF}
  end;
  DeInit;
end;

procedure TStompClient.Init;
begin
  DeInit;
{$IFDEF USESYNAPSE}
  FSynapseTCP := TTCPBlockSocket.Create;
{$ELSE}
  FTCP := TIdTCPClient.Create(nil);
{$ENDIF}
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

function TStompClient.Receive(out StompFrame: IStompFrame; ATimeout: Integer): boolean;
begin
  StompFrame := nil;
  StompFrame := Receive(ATimeout);
  Result := Assigned(StompFrame);
end;

function TStompClient.Receive(ATimeout: Integer): IStompFrame;
{$IFDEF USESYNAPSE}
  function InternalReceiveSynapse(ATimeout: Integer): IStompFrame;
  var
    c: char;
    s: string;
    tout: boolean;
  begin
    tout := False;
    Result := nil;
    try
      try
        FSynapseTCP.SetRecvTimeout(ATimeout);
        s := '';
        try
          while True do
          begin
            c := Chr(FSynapseTCP.RecvByte(ATimeout));
            if FSynapseTCP.LastError = WSAETIMEDOUT then
              raise ESynapseTimeout.Create(FSynapseTCP.LastErrorDesc);
            if c <> CHAR0 then
              s := s + c // should be improved with a string buffer (daniele.teti)
            else
            begin
              c := Chr(FSynapseTCP.RecvByte(ATimeout));
              if FSynapseTCP.LastError = WSAETIMEDOUT then
                raise ESynapseTimeout.Create('' { FSynapseTCP.LastErrorDesc } );
              Break;
            end;
          end;
        except
          on E: ESynapseTimeout do
          begin
            tout := True;
          end;
          on E: Exception do
          begin
            raise ;
          end;
        end;
        if not tout then
        begin
          Result := StompUtils.CreateFrame(s + CHAR0);
        end;
      finally
        s := '';
      end;
    except
      on E: Exception do
      begin
        raise ;
      end;
    end;
  end;
{$ELSE}
  function InternalReceiveINDY(ATimeout: Integer): IStompFrame;
  var
    c: char;
    sb: TStringBuilder;
    tout: boolean;
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
        begin
          Result := StompUtils.CreateFrame(sb.ToString + CHAR0);
        end;
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
{$ENDIF}

begin
{$IFDEF USESYNAPSE}
  Result := InternalReceiveSynapse(ATimeout);
{$ELSE}
  Result := InternalReceiveINDY(ATimeout);
{$ENDIF}
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

procedure TStompClient.Send(QueueOrTopicName: string; TextMessage: string;
  TransactionIdentifier: string; Headers: IStompHeaders);
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
{$IFDEF USESYNAPSE}
  FSynapseTCP.SendString(AFrame.output);
{$ELSE}
  FTCP.IOHandler.write(TEncoding.ASCII.GetBytes(AFrame.output));
{$ENDIF}
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

procedure TStompClient.Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto;
  Headers: IStompHeaders = nil);
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
