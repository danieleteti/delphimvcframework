// Stomp Client for Embarcadero Delphi & FreePascal
// Tested With ApacheMQ 5.2/5.3, Apache Apollo 1.2, RabbitMQ
// Copyright (c) 2009-2015 Daniele Teti
//
// Contributors:
// Daniel Gaspary: dgaspary@gmail.com
// Oliver Marr: oliver.sn@wmarr.de
// Marco Mottadelli: mottadelli75@gmail.com
// WebSite: www.danieleteti.it
// email:d.teti@bittime.it
// *******************************************************

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
  IdHeaderList,

{$ELSE}
  synsock,
  blcksock,

{$ENDIF}
  Classes;

type
  { TStompClient }

  TSenderFrameEvent = procedure(AFrame: IStompFrame) of object;

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
    FServerProtocolVersion: string;
    FClientAcceptProtocolVersion: TStompAcceptProtocol;
    FServer: string;
    FOnBeforeSendFrame: TSenderFrameEvent;
    FOnAfterSendFrame: TSenderFrameEvent;
    procedure SetReceiptTimeout(const Value: Integer);

  protected

{$IFDEF USESYNAPSE}
    procedure SynapseSocketCallBack(Sender: TObject; Reason: THookSocketReason;
      const Value: string);

{$ENDIF}
    procedure Init;
    procedure DeInit;
    procedure MergeHeaders(var AFrame: IStompFrame; var AHeaders: IStompHeaders);
    procedure SendFrame(AFrame: IStompFrame);
    function FormatErrorFrame(const AErrorFrame: IStompFrame): String;
  public
    function SetPassword(const Value: string): IStompClient;
    function SetUserName(const Value: string): IStompClient;
    function Receive(out StompFrame: IStompFrame; ATimeout: Integer): boolean; overload;
    function Receive: IStompFrame; overload;
    function Receive(ATimeout: Integer): IStompFrame; overload;
    procedure Receipt(const ReceiptID: string);
    procedure Connect(Host: string = '127.0.0.1'; Port: Integer = DEFAULT_STOMP_PORT;
      ClientID: string = ''; AcceptVersion: TStompAcceptProtocol = STOMP_Version_1_0);
    procedure Disconnect;
    procedure Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto;
      Headers: IStompHeaders = nil);
    procedure Unsubscribe(Queue: string);
    procedure Send(QueueOrTopicName: string; TextMessage: string; Headers: IStompHeaders = nil);
      overload;
    procedure Send(QueueOrTopicName: string; TextMessage: string; TransactionIdentifier: string;
      Headers: IStompHeaders = nil); overload;
    procedure Ack(const MessageID: string; const TransactionIdentifier: string = '');
    { STOMP 1.1 }
    procedure Nack(const MessageID: string; const TransactionIdentifier: string = '');
    procedure BeginTransaction(const TransactionIdentifier: string);
    procedure CommitTransaction(const TransactionIdentifier: string);
    procedure AbortTransaction(const TransactionIdentifier: string);
    /// ////////////
    constructor Create; virtual;
    destructor Destroy; override;
    function Connected: boolean;
    function SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
    function GetProtocolVersion: string;
    function GetServer: string;
    function GetSession: string;
    property ReceiptTimeout: Integer read FReceiptTimeout write SetReceiptTimeout;
    property Transactions: TStringList read FTransactions;
    // * Manage Events
    property OnBeforeSendFrame: TSenderFrameEvent read FOnBeforeSendFrame write FOnBeforeSendFrame;
    property OnAfterSendFrame: TSenderFrameEvent read FOnAfterSendFrame write FOnAfterSendFrame;
  end;

implementation

{$IFDEF FPC}


const
  CHAR0 = #0;

{$ELSE}


uses
  // Windows,   // Remove windows unit for compiling on ios
  IdGlobal,
  IdGlobalProtocols,
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
  Frame.GetHeaders.Add(TStompHeaders.MESSAGE_ID, MessageID);
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

procedure TStompClient.Connect(Host: string; Port: Integer; ClientID: string;
  AcceptVersion: TStompAcceptProtocol);
var
  Frame: IStompFrame;
begin
  try
    Init;

{$IFDEF USESYNAPSE}
    FSynapseConnected := False;
    FSynapseTCP.Connect(Host, intToStr(Port));
    FSynapseConnected := True;

{$ELSE}
    FTCP.Connect(Host, Port);
    FTCP.IOHandler.MaxLineLength := MaxInt;

{$ENDIF}
    Frame := TStompFrame.Create;
    Frame.SetCommand('CONNECT');

    FClientAcceptProtocolVersion := AcceptVersion;
    if STOMP_Version_1_1 in [FClientAcceptProtocolVersion] then
    begin
      Frame.GetHeaders.Add('heart-beat', '0,1000'); // stomp 1.1
      Frame.GetHeaders.Add('accept-version', '1.1'); // stomp 1.1
    end;

    Frame.GetHeaders.Add('login', FUserName).Add('passcode', FPassword);
    if ClientID <> '' then
      Frame.GetHeaders.Add('client-id', ClientID);
    SendFrame(Frame);
    Frame := nil;
    while Frame = nil do
      Frame := Receive;
    if Frame.GetCommand = 'ERROR' then
      raise EStomp.Create(FormatErrorFrame(Frame));
    if Frame.GetCommand = 'CONNECTED' then
    begin
      FSession := Frame.GetHeaders.Value('session');
      FServerProtocolVersion := Frame.GetHeaders.Value('version'); // stomp 1.1
      FServer := Frame.GetHeaders.Value('server'); // stomp 1.1
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
  Disconnect;
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

function TStompClient.FormatErrorFrame(const AErrorFrame: IStompFrame): String;
begin
  if AErrorFrame.GetCommand <> 'ERROR' then
    raise EStomp.Create('Not an ERROR frame');
  Result := AErrorFrame.GetHeaders.Value('message') + ': ' + AErrorFrame.GetBody;
end;

function TStompClient.GetProtocolVersion: string;
begin
  Result := FServerProtocolVersion;
end;

function TStompClient.GetServer: string;
begin
  Result := FServer;
end;

function TStompClient.GetSession: string;
begin
  Result := FSession;
end;

procedure TStompClient.Init;
begin
  DeInit;

{$IFDEF USESYNAPSE}
  FSynapseTCP := TTCPBlockSocket.Create;
  FSynapseTCP.OnStatus := SynapseSocketCallBack;
  FSynapseTCP.RaiseExcept := True;

{$ELSE}
  FTCP := TIdTCPClient.Create(nil);

{$ENDIF}
  FTransactions := TStringList.Create;
end;

{$IFDEF USESYNAPSE}


procedure TStompClient.SynapseSocketCallBack(Sender: TObject;
  Reason: THookSocketReason; const Value: string);
begin
  // As seen at TBlockSocket.ExceptCheck procedure, it SEEMS safe to say
  // when an error occurred and is not a Timeout, the connection is broken
  if (Reason = HR_Error) and (FSynapseTCP.LastError <> WSAETIMEDOUT)
  then
  begin
    FSynapseConnected := False;
  end;
end;

{$ENDIF}


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

  // If the frame has some content, then set the length of that content.
  if (AFrame.ContentLength > 0) then
    AFrame.GetHeaders.Add('content-length', IntToStr(AFrame.ContentLength));
end;

procedure TStompClient.Nack(const MessageID, TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.SetCommand('NACK');
  Frame.GetHeaders.Add('message-id', MessageID);
  if TransactionIdentifier <> '' then
    Frame.GetHeaders.Add('transaction', TransactionIdentifier);
  SendFrame(Frame);
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
            if c <> CHAR0 then
              s := s + c // should be improved with a string buffer (daniele.teti)
            else
            begin
              c := Chr(FSynapseTCP.RecvByte(ATimeout));
              Break;
            end;
          end;
        except
          on E: ESynapseError do
          begin
            if E.ErrorCode = WSAETIMEDOUT then
              tout := True
            else
              raise;
          end;
          on E: Exception do
          begin
            raise;
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
        raise;
      end;
    end;
  end;

{$ELSE}
  function InternalReceiveINDY(ATimeout: Integer): IStompFrame;
  var
    s: string;
    lSBuilder: TStringBuilder;
    Headers: TIdHeaderList;
    ContentLength: Integer;
    Charset: string;

{$IF CompilerVersion < 24}
    Encoding: TIdTextEncoding;
    FreeEncoding: boolean;
{$ELSE}
    Encoding: IIdTextEncoding;
{$ENDIF}
  begin
    Result := nil;
    lSBuilder := TStringBuilder.Create(1024 * 4);
    try
      FTCP.ReadTimeout := ATimeout;
      FTCP.Socket.DefStringEncoding :=
{$IF CompilerVersion < 24}TIdTextEncoding.UTF8{$ELSE}IndyTextEncoding_UTF8{$ENDIF};

      try
        // read command line
        repeat
          s := FTCP.Socket.ReadLn;
        until s <> '';
        lSBuilder.Append(s + LF);

        // read headers
        Headers := TIdHeaderList.Create(QuotePlain);

        try
          repeat
            s := FTCP.Socket.ReadLn;
            lSBuilder.Append(s + LF);
            if s = '' then
              Break;
            Headers.Add(s);
          until False;

          // read body
          //
          // NOTE: non-text data really should be read as a Stream instead of a String!!!
          //

          if IsHeaderMediaType(Headers.Values['content-type'], 'text') then
          begin
            Charset := Headers.Params['content-type', 'charset'];
            if Charset = '' then
              Charset := 'utf-8';
            Encoding := CharsetToEncoding(Charset);
{$IF CompilerVersion < 24}
            FreeEncoding := True;
{$ENDIF}
          end
          else
          begin
            Encoding := IndyTextEncoding_8Bit();
{$IF CompilerVersion < 24}
            FreeEncoding := False;
{$ENDIF}
          end;

{$IF CompilerVersion < 24}
          try
{$ENDIF}
            if Headers.IndexOfName('content-length') <> -1 then
            begin
              // length specified, read exactly that many bytes
              ContentLength := IndyStrToInt(Headers.Values['content-length']);
              if ContentLength > 0 then
              begin
                s := FTCP.Socket.ReadString(ContentLength, Encoding);
                lSBuilder.Append(s);
              end;
              // frame must still be terminated by a null
              FTCP.Socket.ReadLn(#0);
            end
            else

            begin
              // no length specified, body terminated by frame terminating null
              s := FTCP.Socket.ReadLn(#0, Encoding);
              lSBuilder.Append(s);

            end;
            lSBuilder.Append(#0);
{$IF CompilerVersion < 24}
          finally
            if FreeEncoding then
              Encoding.Free;
          end;
{$ENDIF}
        finally
          Headers.Free;
        end;
      except
        on E: Exception do
        begin
          if lSBuilder.Length > 0 then
            raise EStomp.Create(E.message + sLineBreak + lSBuilder.toString)
          else
            raise;
        end;
      end;
      Result := StompUtils.CreateFrame(lSBuilder.toString);
      if Result.GetCommand = 'ERROR' then
        raise EStomp.Create(FormatErrorFrame(Result));
    finally
      lSBuilder.Free;
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
  if Assigned(FOnBeforeSendFrame) then
    FOnBeforeSendFrame(AFrame);
  FSynapseTCP.SendString(AFrame.output);
  if Assigned(FOnAfterSendFrame) then
    FOnAfterSendFrame(AFrame);

{$ELSE}
  // FTCP.IOHandler.write(TEncoding.ASCII.GetBytes(AFrame.output));
  if Assigned(FOnBeforeSendFrame) then
    FOnBeforeSendFrame(AFrame);

{$IF CompilerVersion < 25}
  FTCP.IOHandler.write(TEncoding.UTF8.GetBytes(AFrame.output));
{$IFEND}
{$IF CompilerVersion >= 25}
  FTCP.IOHandler.write(IndyTextEncoding_UTF8.GetBytes(AFrame.output));
{$IFEND}
  if Assigned(FOnAfterSendFrame) then
    FOnAfterSendFrame(AFrame);

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
