// Stomp Client for Embarcadero Delphi & FreePasca
// Tested With ApacheMQ 5.2/5.3, Apache Apollo 1.2, RabbitMQ
// Copyright (c) 2009-2016 Daniele Teti
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
  DateUtils,

{$IFNDEF USESYNAPSE}
  IdTCPClient,
  IdException,
  IdExceptionCore,
  IdHeaderList,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, // SSL
  System.SyncObjs,
{$ELSE}
  synsock,
  blcksock,

{$ENDIF}
  Classes;

type
  { TStompClient }

  TSenderFrameEvent = procedure(AFrame: IStompFrame) of object;

  THeartBeatThread = class;

  TStompClient = class(TInterfacedObject, IStompClient)
  private

{$IFDEF USESYNAPSE}
    FSynapseTCP: TTCPBlockSocket;
    FSynapseConnected: boolean;

{$ELSE}
    FTCP: TIdTCPClient;
    FIOHandlerSocketOpenSSL : TIdSSLIOHandlerSocketOpenSSL;
{$ENDIF}
    FOnConnect: TStompConnectNotifyEvent; // Add By GC 26/01/2011


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
    FHost: string;
    FPort: Integer;
    FClientID: string;

    FUseSSL   : boolean;      // SSL
    FsslKeyFile : string;     // SSL
    FsslCertFile : string;    // SSL
    FsslKeyPass   : string;   // SSL

    FAcceptVersion: TStompAcceptProtocol;
    FConnectionTimeout: UInt32;
    FOutgoingHeartBeats: Int64;
    FIncomingHeartBeats: Int64;
    FLock: TObject;
    FHeartBeatThread: THeartBeatThread;
    FServerIncomingHeartBeats: Int64;
    FServerOutgoingHeartBeats: Int64;
    FOnHeartBeatError: TNotifyEvent;



    procedure ParseHeartBeat(Headers: IStompHeaders);
    procedure SetReceiptTimeout(const Value: Integer);
    procedure SetConnectionTimeout(const Value: UInt32);
    function GetOnConnect: TStompConnectNotifyEvent;
    procedure SetOnConnect(const Value: TStompConnectNotifyEvent);

  protected

{$IFDEF USESYNAPSE}
    procedure SynapseSocketCallBack(Sender: TObject; Reason: THookSocketReason;
      const Value: string);

{$ENDIF}
    procedure Init;
    procedure DeInit;
    procedure MergeHeaders(var AFrame: IStompFrame;
      var AHeaders: IStompHeaders);
    procedure SendFrame(AFrame: IStompFrame);
    procedure SendHeartBeat;
    function FormatErrorFrame(const AErrorFrame: IStompFrame): string;
    function ServerSupportsHeartBeat: boolean;
    procedure OnHeartBeatErrorHandler(Sender: TObject);
    procedure DoHeartBeatErrorHandler;
    procedure OpenSSLGetPassword(var Password: String);
  public
    Function SetUseSSL(const boUseSSL: boolean;
      const KeyFile : string =''; const CertFile : string = '';
      const PassPhrase : string = ''): IStompClient; // SSL

    function SetPassword(const Value: string): IStompClient;
    function SetUserName(const Value: string): IStompClient;
    function Receive(out StompFrame: IStompFrame; ATimeout: Integer)
      : boolean; overload;
    function Receive: IStompFrame; overload;
    function Receive(ATimeout: Integer): IStompFrame; overload;
    procedure Receipt(const ReceiptID: string);
    procedure Connect(Host: string = '127.0.0.1';
      Port: Integer = DEFAULT_STOMP_PORT; ClientID: string = '';
      AcceptVersion: TStompAcceptProtocol = TStompAcceptProtocol.
      Ver_1_0);
    procedure Disconnect;
    procedure Subscribe(QueueOrTopicName: string;
      Ack: TAckMode = TAckMode.amAuto; Headers: IStompHeaders = nil);
    procedure Unsubscribe(Queue: string; const subscriptionId: string = ''); // Unsubscribe STOMP 1.1 : It requires that the id header matches the id value of previous SUBSCRIBE operation.
    procedure Send(QueueOrTopicName: string; TextMessage: string;
      Headers: IStompHeaders = nil); overload;
    procedure Send(QueueOrTopicName: string; TextMessage: string;
      TransactionIdentifier: string; Headers: IStompHeaders = nil); overload;


    procedure Ack(const MessageID: string; const subscriptionId: string = '';
      const TransactionIdentifier: string = ''); // ACK  STOMP 1.1 : has two REQUIRED headers: message-id, which MUST contain a value matching the message-id for the MESSAGE being acknowledged and subscription, which MUST be set to match the value of the subscription's id header
    { STOMP 1.1 }
    procedure Nack(const MessageID: string; const subscriptionId: string = '';
      const TransactionIdentifier: string = ''); // NACK STOMP 1.1 : takes the same headers as ACK: message-id (mandatory), subscription (mandatory) and transaction (OPTIONAL).
    procedure BeginTransaction(const TransactionIdentifier: string);
    procedure CommitTransaction(const TransactionIdentifier: string);
    procedure AbortTransaction(const TransactionIdentifier: string);
    /// ////////////
    constructor Create; overload; virtual;
    class function CreateAndConnect(Host: string = '127.0.0.1';
      Port: Integer = DEFAULT_STOMP_PORT; ClientID: string = '';
      AcceptVersion: TStompAcceptProtocol = TStompAcceptProtocol.
      Ver_1_0): IStompClient; overload; virtual;
    destructor Destroy; override;
    function SetHeartBeat(const OutgoingHeartBeats, IncomingHeartBeats: Int64): IStompClient;
    function Clone: IStompClient;
    function Connected: boolean;
    function SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
    function GetProtocolVersion: string;
    function GetServer: string;
    function GetSession: string;
    property ReceiptTimeout: Integer read FReceiptTimeout
      write SetReceiptTimeout;
    property Transactions: TStringList read FTransactions;







    property ConnectionTimeout: UInt32 read FConnectionTimeout
      write SetConnectionTimeout;
    // * Manage Events
    property OnBeforeSendFrame: TSenderFrameEvent read FOnBeforeSendFrame
      write FOnBeforeSendFrame;
    property OnAfterSendFrame: TSenderFrameEvent read FOnAfterSendFrame
      write FOnAfterSendFrame;
    property OnHeartBeatError: TNotifyEvent read FOnHeartBeatError write FOnHeartBeatError;

    // Add by GC 26/01/2001
    property OnConnect: TStompConnectNotifyEvent read GetOnConnect write SetOnConnect;
  end;

  THeartBeatThread = class(TThread)
  private
    FStompClient: TStompClient;
    FLock: TObject;
    FOutgoingHeatBeatTimeout: Int64;
    FOnHeartBeatError: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure DoHeartBeatError;
  public
    constructor Create(StompClient: TStompClient; Lock: TObject;
      OutgoingHeatBeatTimeout: Int64); virtual;
    property OnHeartBeatError: TNotifyEvent read FOnHeartBeatError write FOnHeartBeatError;
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
  Character, Winapi.Windows;

{$ENDIF}
{ TStompClient }

procedure TStompClient.AbortTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) > -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.Command := 'ABORT';
    Frame.Headers.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    FInTransaction := False;
    FTransactions.Delete(FTransactions.IndexOf(TransactionIdentifier));
  end
  else
    raise EStomp.CreateFmt
      ('Abort Transaction Error. Transaction [%s] not found',
      [TransactionIdentifier]);
end;

procedure TStompClient.Ack(const MessageID: string; const subscriptionId: string;
  const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'ACK';
  Frame.Headers.Add(TStompHeaders.MESSAGE_ID, MessageID);

  if subscriptionId <> '' then
    Frame.Headers.Add('subscription', subscriptionId);

  if TransactionIdentifier <> '' then
    Frame.Headers.Add('transaction', TransactionIdentifier);
  SendFrame(Frame);
end;

procedure TStompClient.BeginTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) = -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.Command := 'BEGIN';
    Frame.Headers.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    // CheckReceipt(Frame);
    FInTransaction := True;
    FTransactions.Add(TransactionIdentifier);
  end
  else
    raise EStomp.CreateFmt
      ('Begin Transaction Error. Transaction [%s] still open',
      [TransactionIdentifier]);
end;

// procedure TStompClient.CheckReceipt(Frame: TStompFrame);
// var
// ReceiptID: string;
// begin
// if FEnableReceipts then
// begin
// ReceiptID := inttostr(GetTickCount);
// Frame.Headers.Add('receipt', ReceiptID);
// SendFrame(Frame);
// Receipt(ReceiptID);
// end
// else
// SendFrame(Frame);
// end;

function TStompClient.Clone: IStompClient;
begin
  Result := TStompClient.Create;
  Result.SetUserName(FUserName).SetPassword(FPassword);
  TStompClient(Result).ConnectionTimeout := FConnectionTimeout;
  TStompClient(Result).SetHeartBeat(FOutgoingHeartBeats, FIncomingHeartBeats);
  TStompClient(Result).Connect(FHost, FPort, FClientID, FAcceptVersion);

end;

procedure TStompClient.CommitTransaction(const TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  if FTransactions.IndexOf(TransactionIdentifier) > -1 then
  begin
    Frame := TStompFrame.Create;
    Frame.Command := 'COMMIT';
    Frame.Headers.Add('transaction', TransactionIdentifier);
    SendFrame(Frame);
    FInTransaction := False;
    FTransactions.Delete(FTransactions.IndexOf(TransactionIdentifier));
  end
  else
    raise EStomp.CreateFmt
      ('Commit Transaction Error. Transaction [%s] not found',
      [TransactionIdentifier]);
end;

procedure TStompClient.Connect(Host: string; Port: Integer; ClientID: string;
  AcceptVersion: TStompAcceptProtocol);
var
  Frame: IStompFrame;
  lHeartBeat: string;
begin
  FHost := Host;
  FPort := Port;
  FClientID := ClientID;
  FAcceptVersion := AcceptVersion;

  try
    Init;

{$IFDEF USESYNAPSE}
    FSynapseConnected := False;
    FSynapseTCP.Connect(Host, intToStr(Port));
    FSynapseConnected := True;

{$ELSE}


    if FUseSSL then
    begin
      FIOHandlerSocketOpenSSL.OnGetPassword := OpenSSLGetPassword;
      FIOHandlerSocketOpenSSL.Port := 0  ;
      FIOHandlerSocketOpenSSL.DefaultPort := 0       ;
      FIOHandlerSocketOpenSSL.SSLOptions.Method := sslvTLSv1_2; //sslvSSLv3; //sslvSSLv23;
      FIOHandlerSocketOpenSSL.SSLOptions.KeyFile  := FsslKeyFile;
      FIOHandlerSocketOpenSSL.SSLOptions.CertFile := FsslCertFile;
      FIOHandlerSocketOpenSSL.SSLOptions.Mode := sslmUnassigned; //sslmClient;
      FIOHandlerSocketOpenSSL.SSLOptions.VerifyMode := [];
      FIOHandlerSocketOpenSSL.SSLOptions.VerifyDepth := 0;
//      FIOHandlerSocketOpenSSL.OnBeforeConnect := BeforeConnect;
      FTCP.IOHandler := FIOHandlerSocketOpenSSL;
    end
    else
    begin
      FTCP.IOHandler := nil;
    end;

    FTCP.ConnectTimeout := FConnectionTimeout;
    FTCP.Connect(Host, Port);
    FTCP.IOHandler.MaxLineLength := MaxInt;

{$ENDIF}
    Frame := TStompFrame.Create;
    Frame.Command := 'CONNECT';

    FClientAcceptProtocolVersion := AcceptVersion;
    if TStompAcceptProtocol.Ver_1_1 in [FClientAcceptProtocolVersion]
    then
    begin
      Frame.Headers.Add('accept-version', '1.1'); // stomp 1.1
      lHeartBeat := Format('%d,%d', [FOutgoingHeartBeats, FIncomingHeartBeats]);
      Frame.Headers.Add('heart-beat', lHeartBeat); // stomp 1.1
    end
    else
    begin
      Frame.Headers.Add('accept-version', '1.0'); // stomp 1.0
    end;

    Frame.Headers.Add('login', FUserName).Add('passcode', FPassword);
    FClientID := ClientID;
    if ClientID <> '' then
    begin
      Frame.Headers.Add('client-id', ClientID);
    end;
    SendFrame(Frame);
    Frame := nil;
    while Frame = nil do
      Frame := Receive;
    if Frame.Command = 'ERROR' then
      raise EStomp.Create(FormatErrorFrame(Frame));
    if Frame.Command = 'CONNECTED' then
    begin
      FSession := Frame.Headers.Value('session');
      FServerProtocolVersion := Frame.Headers.Value('version'); // stomp 1.1
      FServer := Frame.Headers.Value('server'); // stomp 1.1
      ParseHeartBeat(Frame.Headers);
    end;

    // Let's start the hearbeat thread
    if ServerSupportsHeartBeat then
    begin
      FHeartBeatThread := THeartBeatThread.Create(Self, FLock, FServerOutgoingHeartBeats);
      FHeartBeatThread.OnHeartBeatError := OnHeartBeatErrorHandler;
      FHeartBeatThread.Start;
    end;

    { todo: 'Call event?' -> by Gc}
    // Add by GC 26/01/2011
    if Assigned(FOnConnect) then
      FOnConnect(Self, Frame);

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

{$ELSE}                                           // ClosedGracefully <> FTCP.Connected !!!
  Result := Assigned(FTCP) and FTCP.Connected and (not FTCP.IOHandler.ClosedGracefully);

{$ENDIF}
end;

class function TStompClient.CreateAndConnect(Host: string; Port: Integer;
  ClientID: string; AcceptVersion: TStompAcceptProtocol): IStompClient;
begin
  Result := TStompClient.Create;
  Result.Connect(Host, Port, ClientID, AcceptVersion);
end;

constructor TStompClient.Create;
begin
  inherited;
  FLock := TObject.Create;
  FInTransaction := False;
  FSession := '';
  FUserName := 'guest';
  FPassword := 'guest';
  FUseSSL := false;
  FHeaders := TStompHeaders.Create;
  FTimeout := 200;
  FReceiptTimeout := FTimeout;
  FConnectionTimeout := 1000 * 10; // 10secs
  FIncomingHeartBeats := 10000; // 10secs
  FOutgoingHeartBeats := 0; // disabled
end;

procedure TStompClient.DeInit;
begin

{$IFDEF USESYNAPSE}
  FreeAndNil(FSynapseTCP);

{$ELSE}
  FreeAndNil(FTCP);
  FreeAndNil(FIOHandlerSocketOpenSSL);

{$ENDIF}
  FreeAndNil(FTransactions);
end;

destructor TStompClient.Destroy;
begin
  Disconnect;
  DeInit;
  FLock.Free;

  inherited;
end;

procedure TStompClient.Disconnect;
var
  Frame: IStompFrame;
begin
  if Connected then
  begin
    if ServerSupportsHeartBeat then
    begin
      Assert(Assigned(FHeartBeatThread), 'HeartBeat thread not created');
      FHeartBeatThread.Terminate;
      FHeartBeatThread.WaitFor;
      FHeartBeatThread.Free;
    end;

    Frame := TStompFrame.Create;
    Frame.Command := 'DISCONNECT';

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

procedure TStompClient.DoHeartBeatErrorHandler;
begin
  if Assigned(FOnHeartBeatError) then
  begin
    try
      FOnHeartBeatError(Self);
    except
    end;
  end;
end;

function TStompClient.FormatErrorFrame(const AErrorFrame: IStompFrame): string;
begin
  if AErrorFrame.Command <> 'ERROR' then
    raise EStomp.Create('Not an ERROR frame');
  Result := AErrorFrame.Headers.Value('message') + ': ' +
    AErrorFrame.Body;
end;

function TStompClient.GetOnConnect: TStompConnectNotifyEvent;
begin
  Result := FOnConnect;
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
  FIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
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
  if (Reason = HR_Error) and (FSynapseTCP.LastError <> WSAETIMEDOUT) then
  begin
    FSynapseConnected := False;
  end;
end;

{$ENDIF}


procedure TStompClient.MergeHeaders(var AFrame: IStompFrame;
  var AHeaders: IStompHeaders);
var
  i: Integer;
  h: TKeyValue;
begin
  if Assigned(AHeaders) then
    if AHeaders.Count > 0 then
      for i := 0 to AHeaders.Count - 1 do
      begin
        h := AHeaders.GetAt(i);
        AFrame.Headers.Add(h.Key, h.Value);
      end;

  // If the frame has some content, then set the length of that content.
  if (AFrame.ContentLength > 0) then
    AFrame.Headers.Add('content-length', intToStr(AFrame.ContentLength));
end;

procedure TStompClient.Nack(const MessageID, subscriptionId, TransactionIdentifier: string);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'NACK';
  Frame.Headers.Add('message-id', MessageID);

  if subscriptionId <> '' then
    Frame.Headers.Add('subscription', subscriptionId);

  if TransactionIdentifier <> '' then
    Frame.Headers.Add('transaction', TransactionIdentifier);
  SendFrame(Frame);
end;

procedure TStompClient.OnHeartBeatErrorHandler(Sender: TObject);
begin
  FHeartBeatThread.Terminate;
  FHeartBeatThread.WaitFor;
  FHeartBeatThread.Free;
  FHeartBeatThread := nil;
  Disconnect;
  DoHeartBeatErrorHandler;
end;

procedure TStompClient.OpenSSLGetPassword(var Password: String);
begin
  Password := FsslKeyPass;
end;

procedure TStompClient.ParseHeartBeat(Headers: IStompHeaders);
var
  lValue: string;
  lIntValue: string;
begin
  FServerOutgoingHeartBeats := 0;
  FServerIncomingHeartBeats := 0;
  // WARNING!! server heart beat is reversed
  lValue := Headers.Value('heart-beat');
  if Trim(lValue) <> '' then
  begin
    lIntValue := Fetch(lValue, ',');
    FServerIncomingHeartBeats := StrToInt(lIntValue);
    FServerOutgoingHeartBeats := StrToInt(lValue);
  end;
end;

procedure TStompClient.Receipt(const ReceiptID: string);
var
  Frame: IStompFrame;
begin
  if Receive(Frame, FReceiptTimeout) then
  begin
    if Frame.Command <> 'RECEIPT' then
      raise EStomp.Create('Receipt command error');
    if Frame.Headers.Value('receipt-id') <> ReceiptID then
      raise EStomp.Create('Receipt receipt-id error');
  end;
end;

function TStompClient.Receive(out StompFrame: IStompFrame;
  ATimeout: Integer): boolean;
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
              s := s + c
              // should be improved with a string buffer (daniele.teti)
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
    lLine: string;
    lSBuilder: TStringBuilder;
    Headers: TIdHeaderList;
    ContentLength: Integer;
    Charset: string;
    lHeartBeat: boolean;
    lTimestampFirstReadLn: TDateTime;
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
      FTCP.Socket.ReadTimeout := ATimeout;
      FTCP.Socket.DefStringEncoding :=
{$IF CompilerVersion < 24}TIdTextEncoding.UTF8{$ELSE}IndyTextEncoding_UTF8{$ENDIF};

      try
        lTimestampFirstReadLn := Now;
        // read command line
        while True do
        begin
          lLine := FTCP.Socket.ReadLn(LF, ATimeout, -1,
            FTCP.Socket.DefStringEncoding);

          if FTCP.Socket.ReadLnTimedout then
            Break;

          lHeartBeat := lLine = ''; // here is not timeout because of the previous line

          if FServerProtocolVersion = '1.1' then // 1.1 supports heart-beats
          begin
            if (not lHeartBeat) or (lLine <> '') then
              Break;
            if MilliSecondsBetween(lTimestampFirstReadLn, Now) >= ATimeout then
              Break;
          end
          else
            Break; // 1.0
        end;

        if lLine = '' then
          Exit(nil);
        lSBuilder.Append(lLine + LF);

        // read headers
        Headers := TIdHeaderList.Create(QuotePlain);

        try
          repeat
            lLine := FTCP.Socket.ReadLn;
            lSBuilder.Append(lLine + LF);
            if lLine = '' then
              Break;
            // in case of duplicated header, only the first is considered
            // https://stomp.github.io/stomp-specification-1.1.html#Repeated_Header_Entries
            if Headers.IndexOfName(Fetch(lLine, ':', False, False)) = -1 then
              Headers.Add(lLine);
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
                lLine := FTCP.Socket.ReadString(ContentLength, Encoding);
                lSBuilder.Append(lLine);
              end;
              // frame must still be terminated by a null
              FTCP.Socket.ReadLn(#0 + LF);
            end
            else

            begin
              // no length specified, body terminated by frame terminating null
              lLine := FTCP.Socket.ReadLn(#0 + LF, Encoding);
              lSBuilder.Append(lLine);

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
      if Result.Command = 'ERROR' then
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

procedure TStompClient.Send(QueueOrTopicName: string; TextMessage: string;
  Headers: IStompHeaders);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'SEND';
  Frame.Headers.Add('destination', QueueOrTopicName);
  Frame.Body := TextMessage;
  MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;

procedure TStompClient.Send(QueueOrTopicName: string; TextMessage: string;
  TransactionIdentifier: string; Headers: IStompHeaders);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'SEND';
  Frame.Headers.Add('destination', QueueOrTopicName);
  Frame.Headers.Add('transaction', TransactionIdentifier);
  Frame.Body := TextMessage;
  MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;


procedure TStompClient.SendFrame(AFrame: IStompFrame);
begin
  TMonitor.Enter(FLock);
  Try
    if Connected then // Test if error on Socket
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
  Finally
    TMonitor.Exit(FLock);
  End;
end;

procedure TStompClient.SendHeartBeat;
begin
  TMonitor.Enter(FLock);
  Try
    if Connected then
    begin
        // Winapi.Windows.Beep(600, 200);
    {$IFDEF USESYNAPSE}
        FSynapseTCP.SendString(LF);
    {$ELSE}

    {$IF CompilerVersion < 25}
        FTCP.IOHandler.write(TEncoding.UTF8.GetBytes(LF));
    {$IFEND}
    {$IF CompilerVersion >= 25}
        FTCP.IOHandler.write(IndyTextEncoding_UTF8.GetBytes(LF));
    {$IFEND}

    {$ENDIF}
    end;
  Finally
    TMonitor.Exit(FLock);
  End;
end;

function TStompClient.ServerSupportsHeartBeat: boolean;
begin
  Result := (FServerProtocolVersion = '1.1') and (FServerOutgoingHeartBeats > 0)
end;

procedure TStompClient.SetConnectionTimeout(const Value: UInt32);
begin
  FConnectionTimeout := Value;
end;

function TStompClient.SetHeartBeat(const OutgoingHeartBeats, IncomingHeartBeats: Int64)
  : IStompClient;
begin
  FOutgoingHeartBeats := OutgoingHeartBeats;
  FIncomingHeartBeats := IncomingHeartBeats;
  Result := Self;
end;





procedure TStompClient.SetOnConnect(const Value: TStompConnectNotifyEvent);
begin
  FOnConnect := Value;
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

function TStompClient.SetReceiveTimeout(const AMilliSeconds: Cardinal)
  : IStompClient;
begin
  FTimeout := AMilliSeconds;
  Result := Self;
end;

function TStompClient.SetUserName(const Value: string): IStompClient;
begin
  FUserName := Value;
  Result := Self;
end;

function TStompClient.SetUseSSL(const boUseSSL: boolean; const KeyFile,
  CertFile, PassPhrase: string): IStompClient;
begin
  FUseSSL := boUseSSL;
  FsslKeyFile   := KeyFile;
  FsslCertFile := CertFile;
  FsslKeyPass  := PassPhrase;
  Result := Self;

end;

procedure TStompClient.Subscribe(QueueOrTopicName: string;
  Ack: TAckMode = TAckMode.amAuto; Headers: IStompHeaders = nil);
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'SUBSCRIBE';
  Frame.Headers.Add('destination', QueueOrTopicName)
    .Add('ack', StompUtils.AckModeToStr(Ack));
  if Headers <> nil then
    MergeHeaders(Frame, Headers);
  SendFrame(Frame);
end;

procedure TStompClient.Unsubscribe(Queue: string; const subscriptionId: string = '');
var
  Frame: IStompFrame;
begin
  Frame := TStompFrame.Create;
  Frame.Command := 'UNSUBSCRIBE';
  Frame.Headers.Add('destination', Queue);

  if subscriptionId <> '' then
    Frame.Headers.Add('id', subscriptionId);

  SendFrame(Frame);
end;

{ THeartBeatThread }

constructor THeartBeatThread.Create(StompClient: TStompClient; Lock: TObject;
  OutgoingHeatBeatTimeout: Int64);
begin
  inherited Create(True);
  FStompClient := StompClient;
  FLock := Lock;
  FOutgoingHeatBeatTimeout := OutgoingHeatBeatTimeout;
end;

procedure THeartBeatThread.DoHeartBeatError;
begin
  if Assigned(FOnHeartBeatError) then
  begin
    try
      // TThread.Synchronize(nil,
      // procedure
      // begin
      // FOnHeartBeatError(Self);
      // end);
    except
      // do nothing here
    end;
  end;
end;

procedure THeartBeatThread.Execute;
var
  lStart: TDateTime;
begin
  while not Terminated do
  begin
    lStart := Now;
    while (not Terminated) and (MilliSecondsBetween(Now, lStart) < FOutgoingHeatBeatTimeout) do
    begin
      Sleep(100);
    end;
    if not Terminated then
    begin
      // If the connection is down, the socket is invalidated so
      // it is not necessary to informa the main thread about
      // such kind of disconnection.
      FStompClient.SendHeartBeat;
    end;
  end;
end;

end.
