unit MVCFramework.RESTClient.SystemJSONUtils;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.TypesAliases, MVCFramework.RESTClient;

type
  TSystemJSON = class sealed
  public
    class function BodyAsJSONValue(const RESTResponse: IRESTResponse): TJSONValue;
    class function BodyAsJSONObject(const RESTResponse: IRESTResponse): TJSONObject;
    class function BodyAsJSONArray(const RESTResponse: IRESTResponse): TJSONArray;
    class function JSONValueToString(JSONValue: TJSONValue; const Owns: Boolean = true): String;
  end;

implementation

uses
  MVCFramework.Commons, System.SysUtils;

class function TSystemJSON.BodyAsJSONArray(const RESTResponse: IRESTResponse): TJSONArray;
begin
  Result := TSystemJSON.BodyAsJSONValue(RESTResponse) as TJSONArray;
end;

class function TSystemJSON.BodyAsJSONObject(const RESTResponse: IRESTResponse): TJSONObject;
begin
  Result := TSystemJSON.BodyAsJSONValue(RESTResponse) as TJSONObject;
end;

class function TSystemJSON.BodyAsJSONValue(const RESTResponse: IRESTResponse): TJSONValue;
var
  lBodyAsJSONValue: TJSONValue;
begin
  lBodyAsJSONValue := nil;
  lBodyAsJSONValue := TJSONObject.ParseJSONValue(RESTResponse.BodyAsString);
  if lBodyAsJSONValue = nil then
    raise EMVCException.Create('Invalid JSON');
  Result := lBodyAsJSONValue;
end;

class function TSystemJSON.JSONValueToString(JSONValue: TJSONValue;
  const Owns: Boolean): String;
begin
  Result := JSONValue.ToJSON;
  if Owns then
  begin
    JSONValue.Free;
  end;
end;

end.
