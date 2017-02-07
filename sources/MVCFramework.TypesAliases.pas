unit MVCFramework.TypesAliases;

interface

{$I dmvcframework.inc}


uses
  System.SysUtils
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$IFEND}
    ;

type
{$IFDEF SYSTEMJSON}
  TJSONObject = System.JSON.TJSONObject;
  TJSONValue = System.JSON.TJSONValue;
  TJSONPair = System.JSON.TJSONPair;
  TJSONArray = System.JSON.TJSONArray;
  TJSONNumber = System.JSON.TJSONNumber;
  TJSONNull = System.JSON.TJSONNull;
  TJSONString = System.JSON.TJSONString;
  TJSONTrue = System.JSON.TJSONTrue;
  TJSONFalse = System.JSON.TJSONFalse;
{$ELSE}
  TJSONObject = Data.DBXJSON.TJSONObject;
  TJSONValue = Data.DBXJSON.TJSONValue;
  TJSONPair = Data.DBXJSON.TJSONPair;
  TJSONArray = Data.DBXJSON.TJSONArray;
  TJSONNumber = Data.DBXJSON.TJSONNumber;
  TJSONNull = Data.DBXJSON.TJSONNull;
  TJSONString = Data.DBXJSON.TJSONString;
  TJSONTrue = Data.DBXJSON.TJSONTrue;
  TJSONFalse = Data.DBXJSON.TJSONFalse;
{$ENDIF}

implementation

end.
