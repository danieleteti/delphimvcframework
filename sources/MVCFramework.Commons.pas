unit MVCFramework.Commons;

interface

uses
  System.SysUtils,
  Generics.Collections;

type
  TMVCMimeType = class sealed
  public const
    APPLICATION_JSON = 'application/json';
    TEXT_HTML = 'text/html';
    TEXT_PLAIN = 'text/plain';
    TEXT_XML = 'text/xml';
    TEXT_CSS = 'text/css';
    TEXT_JAVASCRIPT = 'text/javascript';
    IMAGE_JPEG = 'image/jpeg';
    IMAGE_PNG = 'image/x-png';
    TEXT_CACHEMANIFEST = 'text/cache-manifest';
    APPLICATION_OCTETSTREAM = 'application/octet-stream';
    TEXT_EVENTSTREAM = 'text/event-stream';
  end;

  TMVCConstants = class sealed
  public const
    SESSION_TOKEN_NAME = 'dtsessionid';
    DEFAULT_CONTENT_ENCODING = 'utf-8';
    DEFAULT_CONTENT_TYPE = TMVCMimeType.APPLICATION_JSON;
  end;

  EMVCException = class(Exception)
  private
    FHTTPErrorCode: UInt16;
    FApplicationErrorCode: UInt16;
    procedure SetDetailedMessage(const Value: string);

  strict protected
    FDetailedMessage: string;

  public
    constructor Create(const Msg: string);
      overload; virtual;
    constructor Create(const Msg: string; const DetailedMessage: string;
      const ApplicationErrorCode: UInt16;
      const HTTPErrorCode: UInt16 = 500); overload; virtual;
    constructor CreateFmt(const Msg: string; const Args: array of const);
    property HTTPErrorCode: UInt16 read FHTTPErrorCode;
    property DetailedMessage: string read FDetailedMessage
      write SetDetailedMessage;
    property ApplicationErrorCode: UInt16 read FApplicationErrorCode write FApplicationErrorCode;
  end;

  EMVCSessionExpiredException = class(EMVCException)

  end;

  EMVCConfigException = class(EMVCException)

  end;

  EMVCFrameworkView = class(EMVCException)

  end;

  TMVCRequestParamsTable = class(TDictionary<string, string>)

  end;

  TMVCDataObjects = class(TObjectDictionary<string, TObject>)
    constructor Create;
  end;

  TMVCConfig = class sealed
  private
    FConfig: TDictionary<string, string>;
    function GetValue(AIndex: string): string;
    procedure SetValue(AIndex: string; const Value: string);

  public
    constructor Create;
    destructor Destroy; override;
    property Value[AIndex: string]: string read GetValue
      write SetValue; default;
    function ToString: string; override;
    procedure SaveToFile(const AFileName: String);
    procedure LoadFromFile(const AFileName: String);
  end;

{$SCOPEDENUMS ON}


type
  THttpMethod = (GET, POST, PUT, DELETE, HEAD);

function AppPath: string;
function IsReservedOrPrivateIP(const IP: string): boolean;
function IP2Long(IP: string): UInt32;

var
  Lock: TObject;

implementation

uses
  System.IOUtils,
  idGlobal,
  System.StrUtils, Data.DBXJSON;

const
  ReservedIPs: array [1 .. 11] of array [1 .. 2] of string = (
    ('0.0.0.0', '0.255.255.255'),
    ('10.0.0.0', '10.255.255.255'),
    ('127.0.0.0', '127.255.255.255'),
    ('169.254.0.0', '169.254.255.255'),
    ('172.16.0.0', '172.31.255.255'),
    ('192.0.2.0', '192.0.2.255'),
    ('192.88.99.0', '192.88.99.255'),
    ('192.168.0.0', '192.168.255.255'),
    ('198.18.0.0', '198.19.255.255'),
    ('224.0.0.0', '239.255.255.255'),
    ('240.0.0.0', '255.255.255.255')
    );

function IP2Long(IP: string): UInt32;
begin
  Result := idGlobal.IPv4ToDWord(IP);
end;

function IsReservedOrPrivateIP(const IP: string): boolean;
var
  i: Integer;
  IntIP: Cardinal;
begin
  Result := False;
  IntIP := IP2Long(IP);
  for i := low(ReservedIPs) to high(ReservedIPs) do
  begin
    if (IntIP >= IP2Long(ReservedIPs[i][1])) and (IntIP <= IP2Long(ReservedIPs[i][2])) then
    begin
      Exit(True)
    end;
  end;
end;

function AppPath: string;
begin
  Result := TPath.GetDirectoryName(GetModuleName(HInstance));
end;

{ TMVCDataObjects }

constructor TMVCDataObjects.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TMVCConfig }

constructor TMVCConfig.Create;
begin
  inherited;
  FConfig := TDictionary<string, string>.Create;
end;

destructor TMVCConfig.Destroy;
begin
  FConfig.Free;
  inherited;
end;

function TMVCConfig.GetValue(AIndex: string): string;
begin
  if FConfig.ContainsKey(AIndex) then
    Result := FConfig.Items[AIndex]
  else
    raise EMVCConfigException.CreateFmt('Invalid config key [%s]', [AIndex]);
end;

procedure TMVCConfig.LoadFromFile(const AFileName: String);
var
  S: string;
  jobj: TJSONObject;
  p: TJSONPair;
  json: TJSONValue;
  i: Integer;
begin
  S := TFile.ReadAllText(AFileName);
  json := TJSONObject.ParseJSONValue(S);
  if Assigned(json) then
  begin
    if json is TJSONObject then
    begin
      jobj := TJSONObject(json);
      for i := 0 to jobj.Size - 1 do
      begin
        p := jobj.GET(i);
        FConfig.AddOrSetValue(p.JsonString.Value, p.JsonValue.Value);
      end
    end
    else
      raise EMVCConfigException.Create('DMVCFramework configuration file [' + AFileName +
        '] does not contain a valid JSONObject');
  end
  else
    raise EMVCConfigException.Create('Cannot load DMVCFramework configuration file [' + AFileName + ']');
end;

procedure TMVCConfig.SaveToFile(const AFileName: String);
begin
  TFile.WriteAllText(AFileName, ToString, TEncoding.ASCII);
end;

procedure TMVCConfig.SetValue(AIndex: string; const Value: string);
begin
  FConfig.AddOrSetValue(AIndex, Value);
end;

function TMVCConfig.ToString: string;
var
  k: string;
  json: TJSONObject;
begin
  json := TJSONObject.Create;
  try
    for k in FConfig.Keys do
      json.AddPair(k, FConfig[k]);
    Result := json.ToString;
  finally
    json.Free;
  end;
end;

{ EMVCException }

constructor EMVCException.Create(const Msg: string);
begin
  inherited Create(Msg);
  FHTTPErrorCode := 500;
  FDetailedMessage := 'N.A.';
  FApplicationErrorCode := 0;
end;

constructor EMVCException.Create(const Msg, DetailedMessage: string;
  const ApplicationErrorCode: UInt16;
  const HTTPErrorCode: UInt16);
begin
  Create(Msg);
  FHTTPErrorCode := HTTPErrorCode;
  FApplicationErrorCode := ApplicationErrorCode;
  FDetailedMessage := DetailedMessage;
end;

constructor EMVCException.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  inherited;
  FHTTPErrorCode := 500;
  FDetailedMessage := 'N.A.';
  FApplicationErrorCode := 0;
end;

procedure EMVCException.SetDetailedMessage(const Value: string);
begin
  FDetailedMessage := Value;
end;

initialization

Lock := TObject.Create;

finalization

FreeAndNil(Lock);

end.
