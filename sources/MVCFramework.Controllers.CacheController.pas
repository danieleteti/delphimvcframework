// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Controllers.CacheController;

{$I dmvcframework.inc}

interface

// NOTE: To use this controller you need DelphiRedisClient
// To use DelphiRedisClient just open a command prompt, go to where you
// usually put your Delphi libraries and run the following command (requires git)
// git clone --recursive https://github.com/danieleteti/delphiredisclient

uses
  System.SysUtils,
  System.Classes,
  MVCFramework,
  MVCFramework.Commons,
  Redis.Client,
  Redis.Commons,
  Redis.Values;

type

  EMVCCacheException = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCCacheController = class(TMVCController)
  private
    FRedis: IRedisClient;
    FCacheEnabled: Boolean;
    FExposeCache: Boolean;
    FCurrentCacheKey: string;
    procedure SetCacheEnabled(const AValue: Boolean);
    procedure SetExposeCache(const AValue: Boolean);
    procedure CheckCacheKey;
    function RedisClient: IRedisClient;
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionNAme: string; var AHandled: Boolean); override;
    procedure OnAfterAction(AContext: TWebContext; const AActionNAme: string); override;
    /// <summary>
    /// Put in cache an arbitrary string using an arbitraty key (FragmentKey)
    /// </summary>
    procedure SetCacheFragment(const AFragmentKey: string; const AValue: string; const AExpireInSeconds: UInt64);
    /// <summary>
    /// Get a previously cached string present at FragmentKey key
    /// </summary>
    function GetFromCacheFragment(const AFragmentKey: string; out AValue: string): Boolean;
    procedure SetCache(const AExpireInSeconds: UInt64);
    /// <summary>
    /// Sets the cache key that will be used by the subsequent "GetFromCache" and "SetCache" methods
    /// </summary>
    procedure SetCacheKey(const AKey: string);
    /// <summary>
    /// Returns true if the cache is available and automatically fills
    /// the response using the cache contents
    /// </summary>
    function CacheAvailable: Boolean;
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

function TMVCCacheController.CacheAvailable: Boolean;
var
  lOutput: TRedisArray;
  lStatusPieces: TArray<string>;
begin
  if not FCacheEnabled then
    Exit(False); // ignore and return false

  CheckCacheKey;

  // check if the redis key is present
  lOutput := RedisClient.HMGET(FCurrentCacheKey, ['contenttype', 'headers', 'body', 'type', 'status']);
  Result := lOutput.Items[0].HasValue and lOutput.Items[1].HasValue and lOutput.Items[2].HasValue and lOutput.Items[3].HasValue and lOutput.Items[4].HasValue;

  if Result then
  begin
    // if contents is cached, serve it from cache
    Context.Response.CustomHeaders.Clear;
    Context.Response.CustomHeaders.AddStrings(lOutput.Items[1].Value.Split([sLineBreak]));

    if FExposeCache then
      Context.Response.CustomHeaders.AddPair('X-CACHE-HIT', '1');

    ContentType := lOutput.Items[0];

    if lOutput.Items[3] = 'text' then
    begin
      ResponseStream.Append(lOutput.Items[2]);
      RenderResponseStream;
    end
    else
    begin
      Render(TBytesStream.Create(TEncoding.ANSI.GetBytes(lOutput.Items[2])), True);
    end;
    lStatusPieces := string(lOutput.Items[4]).Split([':']);
    ResponseStatus(StrToInt(lStatusPieces[0]), lStatusPieces[1]);
  end;
end;

procedure TMVCCacheController.CheckCacheKey;
begin
  if (FCurrentCacheKey = EmptyStr) then
    raise EMVCCacheException.Create('Cache key not set [Hint: Call "SetCacheKey" before "CacheAvailable" or "SetCache"]');
end;

function TMVCCacheController.GetFromCacheFragment(const AFragmentKey: string;
  out AValue: string): Boolean;
var
  lValue: TRedisString;
begin
  if not FCacheEnabled then
    Exit(False); // ignore and return false

  // check if the redis key is present
  lValue := RedisClient.GET(AFragmentKey);
  Result := lValue.HasValue;
  if Result then
    AValue := lValue;
end;

procedure TMVCCacheController.OnAfterAction(AContext: TWebContext; const AActionNAme: string);
begin
  inherited;
end;

procedure TMVCCacheController.OnBeforeAction(AContext: TWebContext; const AActionNAme: string; var AHandled: Boolean);
begin
  inherited;
  FCacheEnabled := True;
  FExposeCache := True;
end;

function TMVCCacheController.RedisClient: IRedisClient;
var
  lConnection, lKeyAuth: string;
  lPieces: TArray<string>;
begin
  if (FRedis = nil) then
  begin
    lConnection := self.Config['redis_connection_string'];
    lKeyAuth := self.Config['redis_connection_key'];
    if lConnection.Trim.IsEmpty then
      raise ERedisException.Create('"redis_connection_string" config key is not defined (format is <host>:<port>)')
    else
    begin
      lPieces := lConnection.Split([':']);
      if Length(lPieces) <> 2 then
        raise ERedisException.Create('Invalid "redis_connection_string" (format is <host>:<port>)');

      FRedis := NewRedisClient(lPieces[0], StrToInt(lPieces[1]));

      if not String.IsNullOrWhiteSpace(lKeyAuth) then
        FRedis.AUTH(lKeyAuth);
    end;
  end;
  Result := FRedis;
end;

procedure TMVCCacheController.SetCache(const AExpireInSeconds: UInt64);
begin
  if not FCacheEnabled then
    Exit; // ignore

  CheckCacheKey;

  if FCacheEnabled then
    RedisClient.MULTI(
      procedure(const R: IRedisClient)
      var
        SS: TStringStream;
      begin
        if Context.Response.RawWebResponse.ContentStream = nil then
          R.HMSET(FCurrentCacheKey, ['contenttype', 'headers', 'body', 'type', 'status'], [
            ContentType,
            Context.Response.CustomHeaders.Text,
            Context.Response.RawWebResponse.Content,
            'text', Context.Response.StatusCode.ToString + ':' + Context.Response.ReasonString])
        else
        begin
          Context.Response.RawWebResponse.ContentStream.Position := 0;
          SS := TStringStream.Create('', TEncoding.ANSI);
          try
            SS.CopyFrom(Context.Response.RawWebResponse.ContentStream, 0);
            R.HMSET(FCurrentCacheKey, ['contenttype', 'headers', 'body', 'type', 'status'],
              [ContentType,
              Context.Response.CustomHeaders.Text,
              SS.DataString,
              'stream', Context.Response.StatusCode.ToString + ':' +
              Context.Response.ReasonString]);
            Context.Response.RawWebResponse.ContentStream.Position := 0;
          finally
            SS.Free;
          end;
        end;
        R.EXPIRE(FCurrentCacheKey, AExpireInSeconds);
      end);
end;

procedure TMVCCacheController.SetCacheEnabled(const AValue: Boolean);
begin
  FCacheEnabled := AValue;
  FCurrentCacheKey := '';
end;

procedure TMVCCacheController.SetCacheFragment(const AFragmentKey: string; const AValue: string;
const AExpireInSeconds: UInt64);
begin
  if FCacheEnabled then
    RedisClient.&SET(AFragmentKey, TEncoding.Default.GetBytes(AValue), AExpireInSeconds);
end;

procedure TMVCCacheController.SetCacheKey(const AKey: string);
begin
  FCurrentCacheKey := AKey;
end;

procedure TMVCCacheController.SetExposeCache(const AValue: Boolean);
begin
  FExposeCache := AValue;
end;

end.
