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

unit Json.Schema.Field.Objects;

interface

uses
  System.Json,
  System.Generics.Collections,
  Json.Schema.Field,
  Json.Schema.Common.Types;

type
  [ASchemaType(skObject)]
  TJsonFieldObject = class(TJsonField)
  strict private
    fFields: TObjectList<TJsonField>;
    fRef : string;

    function GetFieldsCount: Integer;
    function GetField(const pFieldIndex: Integer): TJsonField;
    function GetFieldByName(const pName: string): TJsonField;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddField(pJsonField: TJsonField);
    procedure RemoveField(pJsonField: TJsonField); overload;
    procedure RemoveField(const pFieldName: string); overload;
    procedure CopyFields(pSourceFieldObject: TJsonFieldObject);

    function CopyField(pSourceField: TJsonField): TJsonField;
    function Clone: TJsonField; overload; override;
    function Clone(pSourceField: TJsonFieldObject): TJsonFieldObject; reintroduce; overload;
    function ToJsonSchema: TJsonObject; override;

    property Fields[const pFieldIndex: Integer]: TJsonField read GetField; default;
    property FieldsByName[const pName: string]: TJsonField read GetFieldByName;
    property FieldsCount: Integer read GetFieldsCount;
    property Ref: string read fRef write fRef;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TJsonFieldObject }

constructor TJsonFieldObject.Create;
begin
  inherited Create;
  fFields := TObjectList<TJsonField>.Create(True);
end;

destructor TJsonFieldObject.Destroy;
begin
  FreeAndNil(fFields);
  inherited Destroy;
end;

function TJsonFieldObject.GetField(const pFieldIndex: Integer): TJsonField;
begin
  Result := nil;
  if not Assigned(fFields[pFieldIndex]) then
    Exit;
  Result := TJsonField(fFields[pFieldIndex])
end;

function TJsonFieldObject.GetFieldByName(const pName: string): TJsonField;
begin
  Result := nil;
  for Result in fFields do
    if (UpperCase(Result.Name) = UpperCase(pName)) then
      Break;
end;

function TJsonFieldObject.GetFieldsCount: Integer;
begin
  Result := fFields.Count;
end;

procedure TJsonFieldObject.RemoveField(pJsonField: TJsonField);
begin
  fFields.Remove(pJsonField);
end;

procedure TJsonFieldObject.RemoveField(const pFieldName: string);
begin
  RemoveField(GetFieldByName(pFieldName));
end;

procedure TJsonFieldObject.AddField(pJsonField: TJsonField);
begin
  fFields.Add(pJsonField);
end;

procedure TJsonFieldObject.CopyFields(pSourceFieldObject: TJsonFieldObject);
var
  vIndex: Integer;
begin
  for vIndex := 0 to pSourceFieldObject.FieldsCount - 1 do
    Self.CopyField(pSourceFieldObject.Fields[vIndex]);
end;

function TJsonFieldObject.CopyField(pSourceField: TJsonField): TJsonField;
begin
  Result := pSourceField.Clone;
  Self.AddField(Result);
end;

function TJsonFieldObject.Clone: TJsonField;
begin
  Result := Self.Clone(Self);
end;

function TJsonFieldObject.Clone(pSourceField: TJsonFieldObject): TJsonFieldObject;
begin
  Result := TJsonFieldObject(inherited Clone);
  Result.CopyFields(pSourceField);
  Result.Ref := pSourceField.Ref;
end;

function TJsonFieldObject.ToJsonSchema: TJsonObject;
const
  c_SchemaRef = '$ref';
  c_PrefixDefinitionName = '#/definitions/';
var
  vJsonObjectList: TJsonObject;
  vJsonRequiredList: TJsonArray;
  vField: TJsonField;
begin
  if (fRef.Length > 0) then
  begin
    Result := TJSONObject.Create;
    Result.AddPair(c_SchemaRef, c_PrefixDefinitionName + fRef);
    Exit;
  end;
  
  Result := inherited ToJsonSchema;

  vJsonRequiredList := TJsonArray.Create;
  try
    vJsonObjectList := TJsonObject.Create;
    for vField in fFields do
    begin
      vJsonObjectList.AddPair(vField.Name, vField.ToJsonSchema);
      if (vField.Required) then
        vJsonRequiredList.Add(vField.Name);
    end;

    if (vJsonRequiredList.Count > 0) then
      Result.AddPair('required', vJsonRequiredList);
    Result.AddPair('properties', vJsonObjectList);
  finally
    if (vJsonRequiredList.Count = 0) then
      FreeAndNil(vJsonRequiredList);
  end;
end;

initialization
  RegisterClass(TJsonFieldObject);

end.
