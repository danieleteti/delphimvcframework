{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Json.Schema.Field.DateTimes;

interface

uses
  System.Json,
  Json.Schema.Field,
  Json.Schema.Common.Types;

type
  [ASchemaType(skDateTime)]
  TJsonFieldDateTime = class(TJsonField)
  strict protected
    const c_DateFormat = 'yyyy-MM-dd';
    const c_TimeFormat = 'HH:mm:ss';
    const c_DateTimeFormat = c_DateFormat + 'T' + c_TimeFormat;

    function GetFormat: string; virtual;
  public
    function ToJsonSchema: TJsonObject; override;
    property Format: string read GetFormat;
  end;

  [ASchemaType(skDate)]
  TJsonFieldDate = class(TJsonFieldDateTime)
  strict protected
    function GetFormat: string; override;
  end;

  [ASchemaType(skTime)]
  TJsonFieldTime = class(TJsonFieldDateTime)
  strict protected
    function GetFormat: string; override;
  end;

implementation

uses
  System.Classes;

{ TJsonFieldDateTime }

function TJsonFieldDateTime.GetFormat: string;
begin
  Result := c_DateTimeFormat;
end;

function TJsonFieldDateTime.ToJsonSchema: TJsonObject;
begin
  Result := inherited ToJsonSchema;
  Result.AddPair('format', GetFormat);
end;

{ TJsonFieldDate }

function TJsonFieldDate.GetFormat: string;
begin
  Result := c_DateFormat;
end;

{ TJsonFieldTime }

function TJsonFieldTime.GetFormat: string;
begin
  Result := c_TimeFormat;
end;

initialization
  RegisterClass(TJsonFieldDateTime);
  RegisterClass(TJsonFieldDate);
  RegisterClass(TJsonFieldTime);

end.
