unit MVCFramework.Controllers.CacheController;

interface

uses
  MVCFramework, Redis.Client, Redis.Commons;

type
  TMVCCacheController = class(TMVCController)
  private
    FRedis: IRedisClient;
    FCacheEnabled: Boolean;
    FExposeCache: Boolean;
    procedure SetCacheEnabled(const Value: Boolean);
    procedure SetExposeCache(const Value: Boolean);
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionNAme: string); override;
    function RedisClient: IRedisClient;
    procedure SetCacheFragment(const Key: string; const Value: string;
      const ExpireInSeconds: UInt64);
    function GetFromCacheFragment(const Key: string; out Value: string): Boolean;
    procedure SetCache(const Key: string; const ExpireInSeconds: UInt64);
    function GetFromCache(const Key: string): Boolean;
    /// <summary>
    /// If set to true the cache is enabled for this controller instance
    /// </summary>
    property CacheEnabled: Boolean read FCacheEnabled write SetCacheEnabled;
    /// <summary>
    /// If set to true the response will contains "X-CACHE-HIT=1" if the content has been
    /// retrived from cache
    /// </summary>
    property ExposeCache: Boolean read FExposeCache write SetExposeCache;
  end;

implementation

uses
  System.SysUtils, System.Classes;

function TMVCCacheController.GetFromCache(const Key: string): Boolean;
var
  lOutput: TArray<string>;
begin
  if not FCacheEnabled then
    Exit(False); // ignore and return false

  // check if the redis key is present
  lOutput := RedisClient.HMGET(Key, ['headers', 'body']);
  Result := not lOutput[0].IsEmpty;
  if Result then
  begin
    // if contents is cached, serve it from cache
    Context.Response.CustomHeaders.Clear;
    Context.Response.CustomHeaders.AddStrings(lOutput[0].Split([sLineBreak]));
    if FExposeCache then
    begin
      Context.Response.CustomHeaders.AddPair('X-CACHE-HIT', '1');
    end;
    ResponseStream.Append(lOutput[1]);
    RenderResponseStream;
  end;
end;

function TMVCCacheController.GetFromCacheFragment(const Key: string; out Value: string): Boolean;
begin
  if not FCacheEnabled then
    Exit(False); // ignore and return false

  // check if the redis key is present
  Result := RedisClient.GET(Key, Value);
end;

procedure TMVCCacheController.OnAfterAction(Context: TWebContext; const AActionNAme: string);
begin
  inherited;
end;

procedure TMVCCacheController.OnBeforeAction(Context: TWebContext; const AActionNAme: string;
  var Handled: Boolean);
begin
  inherited;
  FCacheEnabled := True;
  FExposeCache := True;
end;

function TMVCCacheController.RedisClient: IRedisClient;
var
  lConnection: string;
  lPieces: TArray<string>;
begin
  if FRedis = nil then
  begin
    lConnection := self.Config['redis_connection_string'];
    if lConnection.Trim.IsEmpty then
      FRedis := NewRedisClient // default localhost standard port
    else
    begin
      lPieces := lConnection.Split([':']);
      if Length(lPieces) <> 2 then
        raise ERedisException.Create('Invalid "redis_connection_string" (format is <host>:<port>)');
      FRedis := NewRedisClient(lPieces[0], StrToInt(lPieces[1]));
    end;
  end;
  Result := FRedis;
end;

procedure TMVCCacheController.SetCache(const Key: string; const ExpireInSeconds: UInt64);
begin
  if FCacheEnabled then
    RedisClient.MULTI(
      procedure(R: IRedisClient)
      begin
        R.HMSET(Key, ['headers', 'body'], [Context.Response.CustomHeaders.Text,
          Context.Response.RawWebResponse.Content]);
        R.EXPIRE(Key, 20);
      end);
end;

procedure TMVCCacheController.SetCacheEnabled(const Value: Boolean);
begin
  FCacheEnabled := Value;
end;

procedure TMVCCacheController.SetCacheFragment(const Key: string; const Value: string;
const ExpireInSeconds: UInt64);
begin
  if FCacheEnabled then
  begin
    RedisClient.SETExpireInSec(Key, TEncoding.Default.GetBytes(Value), ExpireInSeconds);
  end;
end;

procedure TMVCCacheController.SetExposeCache(const Value: Boolean);
begin
  FExposeCache := Value;
end;

end.
