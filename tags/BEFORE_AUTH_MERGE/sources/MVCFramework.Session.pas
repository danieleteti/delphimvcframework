unit MVCFramework.Session;

interface

uses System.SysUtils,
  System.Generics.Collections;

type
  TWebSession = class abstract
  strict protected
    FSessionID: string;
    FLastAccess: TDateTime;
    FTimeout: UInt64;
    function GetItems(const Key: string): string; virtual; abstract;
    procedure SetItems(const Key, Value: string); virtual; abstract;

  public
    constructor Create(const SessionID: string; const Timeout: UInt64); virtual;
    destructor Destroy; override;
    procedure MarkAsUsed;
    function ToString: string; override;
    function IsExpired: Boolean;
    property Items[const Key: string]: string read GetItems
      write SetItems; default;
    property SessionID: string read FSessionID;
    property LastAccess: TDateTime read FLastAccess;
    property Timeout: UInt64 read FTimeout;
  end;

  TWebSessionClass = class of TWebSession;

  TWebSessionMemory = class(TWebSession)
  strict protected
    FData: TDictionary<string, string>;
    function GetItems(const Key: string): string; override;
    procedure SetItems(const Key, Value: string); override;

  public
    function ToString: string; override;
    constructor Create(const SessionID: string; const Timeout: UInt64); override;
    destructor Destroy; override;
  end;

  TMVCSessionFactory = class sealed
  protected
    FRegisteredSessionTypes: TDictionary<string, TWebSessionClass>;
    class var FInstance: TMVCSessionFactory;
    constructor Create;
    destructor Destroy; override;

  public
    procedure RegisterSessionType(AName: string; AWebSessionClass: TWebSessionClass);
    class function GetInstance: TMVCSessionFactory;
    function CreateNewByType(AName: string; ASessionID: string; ATimeout: UInt64): TWebSession;
  end;

function SessionList: TObjectDictionary<string, TWebSession>;

implementation

uses
  System.dateutils,
  System.SyncObjs;

var
  GSessionlist: TObjectDictionary<string, TWebSession>;
  GLastSessionListClear: TDateTime;
  CS: TCriticalSection;

function SessionList: TObjectDictionary<string, TWebSession>;
var
  k: string;
begin
  if not Assigned(GSessionlist) then
  begin
    CS.Enter;
    try
      if not Assigned(GSessionlist) then // double check
        GSessionlist := TObjectDictionary<string, TWebSession>.Create([doOwnsValues]);
    finally
      CS.Leave;
    end;
  end;

  if MinutesBetween(now, GLastSessionListClear) >= 1 then
  begin
    TMonitor.Enter(GSessionlist);
    try
      for k in GSessionlist.Keys do
        if GSessionlist.Items[k].IsExpired then
          // if MinutesBetween(now, GSessionlist.Items[k].LastAccess) > 30 then
          GSessionlist.Remove(k);
      GLastSessionListClear := now;
    finally
      TMonitor.Exit(GSessionlist);
    end;
  end;
  Result := GSessionlist;
end;

constructor TWebSession.Create(const SessionID: string; const Timeout: UInt64);
begin
  inherited Create;
  FSessionID := SessionID;
  FTimeout := Timeout;
end;

destructor TWebSession.Destroy;
begin
  inherited;
end;

function TWebSession.IsExpired: Boolean;
begin
  Result := MinutesBetween(now, LastAccess) > FTimeout;
end;

procedure TWebSession.MarkAsUsed;
begin
  FLastAccess := now;
end;

function TWebSession.ToString: string;
begin
  Result := '';
end;

constructor TWebSessionMemory.Create(const SessionID: string; const Timeout: UInt64);
begin
  inherited;
  FData := TDictionary<string, string>.Create;
end;

destructor TWebSessionMemory.Destroy;
begin
  FData.Free;
  inherited;
end;

function TWebSessionMemory.GetItems(const Key: string): string;
begin
  TMonitor.Enter(Self);
  try
    if not FData.TryGetValue(Key, Result) then
      Result := '';
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWebSessionMemory.SetItems(const Key, Value: string);
begin
  TMonitor.Enter(Self);
  try
    FData.AddOrSetValue(Key, Value);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TWebSessionMemory.ToString: string;
var
  Key: string;
begin
  Result := '';
  for Key in FData.Keys do
  begin
    Result := Key + ' = ' + QuotedStr(FData.Items[Key]) + sLineBreak;
  end;
end;

{ TMVCSessionManager }

constructor TMVCSessionFactory.Create;
begin
  inherited;
  FRegisteredSessionTypes := TDictionary<string, TWebSessionClass>.Create;
end;

function TMVCSessionFactory.CreateNewByType(AName: string; ASessionID: string; ATimeout: UInt64)
  : TWebSession;
var
  clazz: TWebSessionClass;
begin
  if not FRegisteredSessionTypes.TryGetValue(AName, clazz) then
    raise Exception.Create('Unknown session type');
  Result := clazz.Create(ASessionID, ATimeout);
end;

destructor TMVCSessionFactory.Destroy;
begin
  FRegisteredSessionTypes.Free;
  inherited;
end;

class
  function TMVCSessionFactory.GetInstance: TMVCSessionFactory;
begin
  if not Assigned(FInstance) then
    FInstance := TMVCSessionFactory.Create;
  Result := FInstance;
end;

procedure TMVCSessionFactory.RegisterSessionType(AName: string; AWebSessionClass: TWebSessionClass);
begin
  FRegisteredSessionTypes.AddOrSetValue(AName, AWebSessionClass);
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('memory', TWebSessionMemory);
CS := TCriticalSection.Create;

finalization

TMVCSessionFactory.FInstance.Free;
FreeAndNil(GSessionlist);
FreeAndNil(CS);

end.
