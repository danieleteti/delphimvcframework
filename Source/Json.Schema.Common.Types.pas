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

unit Json.Schema.Common.Types;

interface

type
  TSchemaKind = (skUnknown, skInteger, skInt64, skNumber, skDateTime, skDate, skTime, skEnumeration, skBoolean,
     skObject, skArray, skString, skChar, skGuid);

  ASchemaType = class(TCustomAttribute)
  strict private
    fKind: TSchemaKind;

    const c_SchemaTypeBoolean = 'boolean';
    const c_SchemaTypeInteger = 'integer';
    const c_SchemaTypeNumber = 'number';
    const c_SchemaTypeString = 'string';
    const c_SchemaTypeArray = 'array';
    const c_SchemaTypeObject = 'object';

    function GetName: string;
  public
    constructor Create(const pKind: TSchemaKind);
    property Name: string read GetName;
    property Kind: TSchemaKind read fKind;
  end;

implementation

uses
  System.SysUtils;

{ ASchemaType }

constructor ASchemaType.Create(const pKind: TSchemaKind);
begin
  inherited Create;
  fKind := pKind;
end;

function ASchemaType.GetName: string;
begin
  Result := EmptyStr;
  case fKind of
    skInteger, skInt64, skEnumeration: Result := c_SchemaTypeInteger;
    skNumber: Result := c_SchemaTypeNumber;
    skString, skChar, skGuid, skDateTime, skDate, skTime: Result := c_SchemaTypeString;
    skBoolean: Result := c_SchemaTypeBoolean;
    skObject: Result := c_SchemaTypeObject;
    skArray: Result := c_SchemaTypeArray;
  end;
end;

end.
