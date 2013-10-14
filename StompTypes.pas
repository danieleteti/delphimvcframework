// Stomp Client for Embarcadero Delphi & FreePascal
// Tested With ApacheMQ 5.2/5.3
// Copyright (c) 2009-2013 Daniele Teti
//
// Contributors:
// Daniel Gaspary: dgaspary@gmail.com
// Oliver Marr: oliver.sn@wmarr.de
// WebSite: www.danieleteti.it
// email:d.teti@bittime.it
// *******************************************************

unit StompTypes;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils,
  Classes;

const
  LINE_END: char = #10;
  COMMAND_END: char = #0;
  DEFAULT_STOMP_PORT = 61613;

type
  TAckMode = (amAuto, amClient, amClientIndividual { STOMP 1.1 } );

  TStompAcceptProtocol = (STOMP_Version_1_0, STOMP_Version_1_1);

  EStomp = class(Exception)
  end;

  EStompDisconnectionError = class(EStomp)

  end;

  TKeyValue = record
    Key: string;
    Value: string;
  end;

  PKeyValue = ^TKeyValue;

  IStompHeaders = interface
    ['{BD087D9D-0576-4C35-88F9-F5D6348E3894}']
    function Add(Key, Value: string): IStompHeaders; overload;
    function Add(HeaderItem: TKeyValue): IStompHeaders; overload;
    function Value(Key: string): string;
    function Remove(Key: string): IStompHeaders;
    function IndexOf(Key: string): Integer;
    function Count: Cardinal;
    function GetAt(const index: Integer): TKeyValue;
    function Output: string;
  end;

  IStompFrame = interface
    ['{68274885-D3C3-4890-A058-03B769B2191E}']
    function Output: string;
    procedure SetHeaders(const Value: IStompHeaders);
    function GetCommand: string;
    procedure SetCommand(const Value: string);
    function GetBody: string;
    procedure SetBody(const Value: string);
    function GetHeaders: IStompHeaders;
  end;

  IStompClient = interface
    ['{EDE6EF1D-59EE-4FCC-9CD7-B183E606D949}']
    function Receive(out StompFrame: IStompFrame; ATimeout: Integer): Boolean; overload;
    function Receive: IStompFrame; overload;
    function Receive(ATimeout: Integer): IStompFrame; overload;
    procedure Receipt(const ReceiptID: string);
    procedure Connect(Host: string = '127.0.0.1'; Port: Integer = 61613; ClientID: string = '';
      AcceptVersion: TStompAcceptProtocol = STOMP_Version_1_0);
    procedure Disconnect(WithoutSendingFrame: Boolean = false);
    procedure Subscribe(QueueOrTopicName: string; Ack: TAckMode = amAuto;
      Headers: IStompHeaders = nil);
    procedure Unsubscribe(Queue: string);
    procedure Send(QueueOrTopicName: string; TextMessage: string;
      Headers: IStompHeaders = nil); overload;
    procedure Send(QueueOrTopicName: string; TextMessage: string; TransactionIdentifier: string;
      Headers: IStompHeaders = nil); overload;
    procedure Ack(const MessageID: string; const TransactionIdentifier: string = '');
    { STOMP 1.1 }
    procedure Nack(const MessageID: string; const TransactionIdentifier: string = '');
    procedure BeginTransaction(const TransactionIdentifier: string);
    procedure CommitTransaction(const TransactionIdentifier: string);
    procedure AbortTransaction(const TransactionIdentifier: string);
    /// ////////////
    function SetPassword(const Value: string): IStompClient;
    function SetUserName(const Value: string): IStompClient;
    function SetReceiveTimeout(const AMilliSeconds: Cardinal): IStompClient;
    function Connected: Boolean;
    function GetProtocolVersion: string;
    function GetServer: string;
    function GetSession: string;
  end;

  TStompHeaders = class(TInterfacedObject, IStompHeaders)
  private
    FList: TList;
    function GetItems(index: Cardinal): TKeyValue;
    procedure SetItems(index: Cardinal; const Value: TKeyValue);

  public
    class function NewDurableSubscriptionHeader(const SubscriptionName: string): TKeyValue;
    class function NewPersistentHeader(const Value: Boolean): TKeyValue;
    class function NewReplyToHeader(const DestinationName: string): TKeyValue;
    /// /////////////////////////////////////////////7
    function Add(Key, Value: string): IStompHeaders; overload;
    function Add(HeaderItem: TKeyValue): IStompHeaders; overload;
    function Value(Key: string): string;
    function Remove(Key: string): IStompHeaders;
    function IndexOf(Key: string): Integer;
    function Count: Cardinal;
    function GetAt(const index: Integer): TKeyValue;
    constructor Create;
    destructor Destroy; override;
    function Output: string;
    property Items[index: Cardinal]: TKeyValue read GetItems write SetItems; default;
  end;

  TStompFrame = class(TInterfacedObject, IStompFrame)
  private
    FCommand: string;
    FBody   : string;
    FHeaders: IStompHeaders;
    procedure SetHeaders(const Value: IStompHeaders);
    function GetCommand: string;
    procedure SetCommand(const Value: string);
    function GetBody: string;
    procedure SetBody(const Value: string);
    function GetHeaders: IStompHeaders;

  public
    constructor Create;
    destructor Destroy; override;
    property Command: string read GetCommand write SetCommand;
    property Body: string read GetBody write SetBody;
    // return '', when Key doesn't exist or Value of Key is ''
    // otherwise, return Value;
    function Output: string;
    property Headers: IStompHeaders read GetHeaders write SetHeaders;
  end;

  TAddress = record
    Host: string;
    Port: Integer;
    UserName: string;
    Password: string;
  end;

  TAddresses = array of TAddress;

  IStompListener = interface
    ['{CB3EB297-8616-408E-A0B2-7CCC11224DBC}']
    procedure StopListening;
  end;

  IStompClientListener = interface
    ['{C4C0D932-8994-43FB-9D32-A03FE86AEFE4}']
    procedure OnMessage(StompClient: IStompClient; StompFrame: IStompFrame;
      var StompListening: Boolean);
    procedure OnStopListen(StompClient: IStompClient);
  end;

  TStompClientListener = class(TThread, IStompListener)
  strict protected
    FStompClientListener: IStompClientListener;
    FStompClient        : IStompClient;
    procedure Execute; override;

  public
    constructor Create(StompClient: IStompClient; StompClientListener: IStompClientListener);
    procedure StopListening;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  end;

type
  StompUtils = class
    class function StripLastChar(Buf: string; LastChar: char): string;
    class function CreateFrame(Buf: string): TStompFrame;
    class function AckModeToStr(AckMode: TAckMode): string;
    class function NewHeaders: IStompHeaders;
    class function NewFrame: IStompFrame;
    class function TimestampAsDateTime(const HeaderValue: string): TDateTime;
    class function NewStomp(Host: string = '127.0.0.1'; Port: Integer = DEFAULT_STOMP_PORT;
      ClientID      : string = '';
      const UserName: string = 'guest'; const Password: string = 'guest';
      AcceptVersion : TStompAcceptProtocol = STOMP_Version_1_0): IStompClient;
  end;

implementation

uses
  Dateutils,
  StompClient;

class function StompUtils.NewStomp(Host: string = '127.0.0.1'; Port: Integer = DEFAULT_STOMP_PORT;
  ClientID     : string = ''; const UserName: string = 'guest'; const Password: string = 'guest';
  AcceptVersion: TStompAcceptProtocol = STOMP_Version_1_0): IStompClient;
begin
  Result := TStompClient.Create;
  Result.SetUserName(UserName);
  Result.SetPassword(Password);
  Result.Connect(Host, Port, ClientID, AcceptVersion);
end;

class function StompUtils.StripLastChar(Buf: string; LastChar: char): string;
var
  p: Integer;
begin
  p := Pos(COMMAND_END, Buf);
  if (p = 0) then
    raise EStomp.Create('frame no ending');
  Result := Copy(Buf, 1, p - 1);
end;

class function TStompHeaders.NewDurableSubscriptionHeader(const SubscriptionName: string)
  : TKeyValue;
begin
  Result.Key := 'activemq.subscriptionName';
  Result.Value := SubscriptionName;
end;

class function TStompHeaders.NewPersistentHeader(const Value: Boolean): TKeyValue;
begin
  Result.Key := 'persistent';
  Result.Value := LowerCase(BoolToStr(Value, true));
end;

class function TStompHeaders.NewReplyToHeader(const DestinationName: string): TKeyValue;
begin
  Result.Key := 'reply-to';
  Result.Value := DestinationName;
end;

class function StompUtils.NewHeaders: IStompHeaders;
begin
  Result := TStompHeaders.Create;
end;

class function StompUtils.TimestampAsDateTime(const HeaderValue: string): TDateTime;
begin
  Result := EncodeDateTime(1970, 1, 1, 0, 0, 0, 0) + StrToInt64(HeaderValue) / 86400000;
end;

class function StompUtils.AckModeToStr(AckMode: TAckMode): string;
begin
  case AckMode of
    amAuto:
      Result := 'auto';
    amClient:
      Result := 'client';
    amClientIndividual:
      Result := 'client-individual'; // stomp 1.1
    else
      raise EStomp.Create('Unknown AckMode');
  end;
end;

constructor TStompFrame.Create;
begin
  FHeaders := TStompHeaders.Create;
  self.FCommand := '';
  self.FBody := '';
end;

destructor TStompFrame.Destroy;
begin
  inherited;
end;

function TStompFrame.GetBody: string;
begin
  Result := FBody;
end;

function TStompFrame.GetCommand: string;
begin
  Result := FCommand;
end;

function TStompFrame.GetHeaders: IStompHeaders;
begin
  Result := FHeaders;
end;

function TStompFrame.Output: string;
begin
  Result := FCommand + LINE_END + FHeaders.Output + LINE_END + FBody + COMMAND_END;
end;

procedure TStompFrame.SetBody(const Value: string);
begin
  FBody := Value;
end;

procedure TStompFrame.SetCommand(const Value: string);
begin
  FCommand := Value;
end;

procedure TStompFrame.SetHeaders(const Value: IStompHeaders);
begin
  FHeaders := Value;
end;

function GetLine(Buf: string; var From: Integer): string;
var
  i: Integer;
begin
  if (From > Length(Buf)) then
    raise EStomp.Create('From out of bound.');

  i := From;

  while (i <= Length(Buf)) do
  begin
    if (Buf[i] <> LINE_END) then
      inc(i)
    else
      break;
  end;

  if (Buf[i] = LINE_END) then
  begin
    Result := Copy(Buf, From, i - From);
    From := i + 1;
    exit;
  end
  else
    raise EStomp.Create('End of Line not found.');
end;

class function StompUtils.CreateFrame(Buf: string): TStompFrame;
var
  line      : string;
  i         : Integer;
  p         : Integer;
  Key, Value: string;
  other     : string;
  contLen   : Integer;
  sContLen  : string;
begin
  Result := TStompFrame.Create;
  i := 1;
  try
    Result.Command := GetLine(Buf, i);
    while true do
    begin
      line := GetLine(Buf, i);
      if (line = '') then
        break;
      p := Pos(':', line);
      if (p = 0) then
        raise Exception.Create('header line error');
      Key := Copy(line, 1, p - 1);
      Value := Copy(line, p + 1, Length(line) - p);
      Result.Headers.Add(Key, Value);
    end;
    other := Copy(Buf, i, high(Integer));
    sContLen := Result.Headers.Value('content-length');
    if (sContLen <> '') then
    begin
      if other[Length(other)] <> #0 then
        raise EStomp.Create('frame no ending');
      contLen := StrToInt(sContLen);
      other := StripLastChar(other, COMMAND_END);
      if Length(other) <> contLen then // there is still the command_end
        raise EStomp.Create('frame too short');
      Result.Body := other;
    end
    else
    begin
      Result.Body := StripLastChar(other, COMMAND_END)
    end;
  except
    on EStomp do
    begin
      // ignore
      Result.Free;
      Result := nil;
    end;
    on e: Exception do
    begin
      Result.Free;
      raise EStomp.Create(e.message);
    end;
  end;
end;

class function StompUtils.NewFrame: IStompFrame;
begin
  Result := TStompFrame.Create;
end;

{ TStompHeaders }

function TStompHeaders.Add(Key, Value: string): IStompHeaders;
var
  p: PKeyValue;
begin
  New(p);
  p^.Key := Key;
  p^.Value := Value;
  FList.Add(p);
  Result := self;
end;

function TStompHeaders.Add(HeaderItem: TKeyValue): IStompHeaders;
begin
  Result := Add(HeaderItem.Key, HeaderItem.Value);
end;

function TStompHeaders.Count: Cardinal;
begin
  Result := FList.Count;
end;

constructor TStompHeaders.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TStompHeaders.Destroy;
var
  i: Integer;
begin
  if FList.Count > 0 then
    for i := 0 to FList.Count - 1 do
      Dispose(PKeyValue(FList[i]));
  FList.Free;
  inherited;
end;

function TStompHeaders.GetAt(const index: Integer): TKeyValue;
begin
  Result := GetItems(index)
end;

function TStompHeaders.GetItems(index: Cardinal): TKeyValue;
begin
  Result := PKeyValue(FList[index])^;
end;

function TStompHeaders.IndexOf(Key: string): Integer;
var
  i: Integer;
begin
  Result := - 1;
  for i := 0 to FList.Count - 1 do
  begin
    if GetItems(i).Key = Key then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TStompHeaders.Output: string;
var
  i : Integer;
  kv: TKeyValue;
begin
  Result := '';
  if FList.Count > 0 then
    for i := 0 to FList.Count - 1 do
    begin
      kv := Items[i];
      Result := Result + kv.Key + ':' + kv.Value + LINE_END;
    end
  else
    Result := LINE_END;
end;

function TStompHeaders.Remove(Key: string): IStompHeaders;
var
  p: Integer;
begin
  p := IndexOf(Key);
  Dispose(PKeyValue(FList[p]));
  FList.Delete(p);
  Result := self;
end;

procedure TStompHeaders.SetItems(index: Cardinal; const Value: TKeyValue);
var
  p: Integer;
begin
  p := IndexOf(Value.Key);
  if p > - 1 then
  begin
    PKeyValue(FList[p])^.Key := Value.Key;
    PKeyValue(FList[p])^.Value := Value.Value;
  end
  else
    raise EStomp.Create('Error SetItems');
end;

function TStompHeaders.Value(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  i := IndexOf(Key);
  if i > - 1 then
    Result := GetItems(i).Value;
end;

{ TStompListener }

constructor TStompClientListener.Create(StompClient: IStompClient;
  StompClientListener: IStompClientListener);
begin
  inherited Create(true);
  FStompClientListener := StompClientListener;
  FStompClient := StompClient;
  Resume;
end;

procedure TStompClientListener.Execute;
var
  frame     : IStompFrame;
  StopListen: Boolean;
begin
  StopListen := false;
  while (not terminated) and (not StopListen) do
  begin
    try
      if FStompClient.Receive(frame, 2000) then
      begin
        try
          FStompClientListener.OnMessage(FStompClient, frame, StopListen);
        except

        end;
      end;
    except
      on e: EStompDisconnectionError do
      begin
        try
          StopListen := true;
          FStompClientListener.OnStopListen(FStompClient);
        except
        end;
      end;
    end;
  end;
end;

function TStompClientListener.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin

end;

procedure TStompClientListener.StopListening;
begin
  Terminate;
  WaitFor;
  try
    FStompClientListener.OnStopListen(FStompClient);
  except
  end;
end;

function TStompClientListener._AddRef: Integer;
begin

end;

function TStompClientListener._Release: Integer;
begin

end;

end.
