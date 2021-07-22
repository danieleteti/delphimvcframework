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

unit Json.Schema;

interface

uses
  System.SysUtils,
  System.Json,
  Json.Schema.Common.Types,
  Json.Schema.Field,
  Json.Schema.Field.Objects;

type
  ETypeNotSupportedByAField = class(Exception);

  TJsonSchema = class(TObject)
  strict private
    fRoot: TJsonFieldObject;
    fRef : string;
    const c_ErrorTypeNotSupportedByAField = 'Type not supported by a field.';

    function GetJsonFieldClass<T>: TJsonFieldClass;
    function GetSchemaKind<T>: TSchemaKind;
  private
    procedure SetRef(const Value: string);
    function GetRef: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function ToJson: TJsonObject;
    function Clone: TJsonSchema; overload;
    function Clone(pSourceField: TJsonSchema): TJsonSchema; overload;
    function AddField<T>(const pName: string; const pDescription: string = ''): TJsonField; overload;
    function AddField(pSchemaObject: TJsonSchema): TJsonField; overload;
    function AddFieldAsType<T>(const pName: string; const pDescription: string = ''): T;

    property Root: TJsonFieldObject read fRoot;
    property Ref: string read GetRef write SetRef;
  end;

implementation

uses
  System.Classes,
  System.TypInfo,
  System.Rtti,
  Json.Schema.Field.Strings,
  Json.Schema.Field.Enums,
  Json.Schema.Field.Numbers,
  Json.Schema.Field.Arrays,
  Json.Schema.Field.DateTimes,
  Json.Schema.Field.Booleans;

{ TJsonSchema }

constructor TJsonSchema.Create;
begin
  inherited Create;
  fRoot := TJsonFieldObject.Create;
end;

destructor TJsonSchema.Destroy;
begin
  FreeAndNil(fRoot);
  inherited Destroy;
end;

function TJsonSchema.GetJsonFieldClass<T>: TJsonFieldClass;
var
  vSchemaKind: TSchemaKind;
  vClass: TPersistentClass;
begin
  Result := nil;
  vSchemaKind := GetSchemaKind<T>;
  if (vSchemaKind = skUnknown) then
    raise ETypeNotSupportedByAField.Create(c_ErrorTypeNotSupportedByAField);

  case vSchemaKind of
    skObject:
    begin
      vClass := FindClass(string(PTypeInfo(System.TypeInfo(T))^.Name));
      if not vClass.InheritsFrom(TJsonField) then
        raise ETypeNotSupportedByAField.Create(c_ErrorTypeNotSupportedByAField);
      Result := TJsonFieldClass(vClass);
    end;
    skInteger: Result := TJsonFieldInteger;
    skInt64: Result := TJsonFieldInt64;
    skNumber: Result := TJsonFieldNumber;
    skDateTime: Result := TJsonFieldDateTime;
    skDate: Result := TJsonFieldDate;
    skTime: Result := TJsonFieldTime;
    skEnumeration: Result := TJsonFieldEnum;
    skBoolean: Result := TJsonFieldBoolean;
    skArray: Result := TJsonFieldArray;
    skString, skChar: Result := TJsonFieldString;
    skGuid: Result := TJsonFieldGuid;
  else
    raise ETypeNotSupportedByAField.Create(c_ErrorTypeNotSupportedByAField);
  end;
end;

procedure TJsonSchema.SetRef(const Value: string);
begin
  fRoot.Ref := Value;
end;

function TJsonSchema.GetSchemaKind<T>: TSchemaKind;
var
  vTypeInfo: PTypeInfo;
  vClass: TPersistentClass;
begin
  Result := skUnknown;
  vTypeInfo := System.TypeInfo(T);
  if not Assigned(vTypeInfo) then
    Exit;

  case vTypeInfo^.Kind of
    tkClass: Result := skObject;
    tkArray: Result := skArray;
    tkString, tkUString, tkChar: Result := skString;
    tkRecord:
    begin
      if (LowerCase(string(vTypeInfo^.Name)) = 'tguid') then
        Result := skGuid
    end;
    tkInteger: Result := skInteger;
    tkInt64: Result := skInt64;
    tkEnumeration:
    begin
      if (LowerCase(string(vTypeInfo^.Name)) = 'boolean') then
        Result := skBoolean
      else
        Result := skEnumeration;
    end;
    tkFloat:
    begin
      if (LowerCase(string(vTypeInfo^.Name)) = 'tdatetime') then
        Result := skDateTime
      else if (LowerCase(string(vTypeInfo^.Name)) = 'tdate') then
        Result := skDate
      else if (LowerCase(string(vTypeInfo^.Name)) = 'ttime') then
        Result := skTime
      else
        Result := skNumber;
    end;
  end;
end;

function TJsonSchema.GetRef: string;
begin
  Result := FRef;
end;

function TJsonSchema.Clone: TJsonSchema;
begin
  Result := Self.Clone(Self);
end;

function TJsonSchema.AddField(pSchemaObject: TJsonSchema): TJsonField;
begin
  Result := pSchemaObject.Root.Clone;
  Self.Root.AddField(Result);
end;

function TJsonSchema.AddField<T>(const pName: string; const pDescription: string = ''): TJsonField;
begin
  Result := GetJsonFieldClass<T>.Create;
  Result.Name := pName;
  Result.Description := pDescription;
  fRoot.AddField(Result);
end;

function TJsonSchema.AddFieldAsType<T>(const pName, pDescription: string): T;
var
  vValue: TValue;
begin
  vValue := AddField<T>(pName, pDescription);
  Result := vValue.AsType<T>;
end;

function TJsonSchema.Clone(pSourceField: TJsonSchema): TJsonSchema;
begin
  Result := TJsonSchema.Create;
  Result.Root.CopyFields(pSourceField.Root);
  Result.Root.Ref := pSourceField.Root.Ref;
end;

function TJsonSchema.ToJson: TJsonObject;
begin
  if fRef.Length = 0 then
    Result := fRoot.ToJsonSchema
  else
  begin
    Result := TJSONObject.Create;
    Result.AddPair('$ref', fRef);
  end;
end;

end.
