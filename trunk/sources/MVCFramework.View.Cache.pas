unit MVCFramework.View.Cache;

interface

uses
  Generics.Collections;

type
  TViewCache = class
  protected
    type
    TViewCacheItem = class
      ValueTimeStamp: TDateTime;
      Value: string;
    end;

  var
    FDictionary: TObjectDictionary<string, TViewCacheItem>;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Put(Key: string; Value: string);
    function IsAvailable(Key: string; DateTime: TDateTime; out Value: string): boolean;
  end;

implementation

uses
  System.SysUtils;

{ TViewCache }

constructor TViewCache.Create;
begin
  inherited;
  FDictionary := TObjectDictionary<string, TViewCacheItem>.Create([doOwnsValues]);
end;

destructor TViewCache.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

function TViewCache.IsAvailable(Key: string; DateTime: TDateTime; out Value: string): boolean;
var
  element: TViewCacheItem;
begin
  Result := FDictionary.TryGetValue(Key, element);
  if Result and (element.ValueTimeStamp > DateTime) then
    Value := element.Value;
end;

procedure TViewCache.Put(Key, Value: string);
var
  s      : string;
  element: TViewCacheItem;
begin
  TMonitor.Enter(self);
  try
    if IsAvailable(Key, 0, s) then
    begin
      FDictionary.Remove(Key);
    end;
    element := TViewCacheItem.Create;
    FDictionary.Add(Key, element);
    element.Value := Value;
    element.ValueTimeStamp := now;
  finally
    TMonitor.Exit(self);
  end;
end;

end.
