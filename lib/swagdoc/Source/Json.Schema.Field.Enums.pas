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

unit Json.Schema.Field.Enums;

interface

uses
  System.Json,
  System.Generics.Collections,
  Json.Schema.Field,
  Json.Schema.Common.Types;

type
  TJsonFieldEnumType = (etNumber, etString);

  TJsonFieldEnumItem = class
  strict private
    fValue: Integer;
    fDescription: string;
  public
    constructor Create(const pValue: Integer; const pDescription: string); reintroduce;

    property Value: Integer read fValue;
    property Description: string read fDescription;
  end;

  [ASchemaType(skEnumeration)]
  TJsonFieldEnum = class(TJsonField)
  strict private
    fJsonEnumItems: TObjectList<TJsonFieldEnumItem>;
    fEnumType: TJsonFieldEnumType;

    function GetCount: Integer;
    function GetItem(const pIndex: Byte): TJsonFieldEnumItem;
  strict protected
    function GetTypeName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddItem(const pValue: Byte; const pDescription: string);
    procedure AddItems(const pItemsDescriptions: array of string);

    function ToJsonSchema: TJsonObject; override;

    function Clone: TJsonField; override;

    property Count: Integer read GetCount;
    property Items[const pIndex: Byte]: TJsonFieldEnumItem read GetItem;
    property EnumType: TJsonFieldEnumType read fEnumType write fEnumType;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TJsonFieldEnumItem }

constructor TJsonFieldEnumItem.Create(const pValue: Integer; const pDescription: string);
begin
  inherited Create;
  fValue := pValue;
  fDescription := pDescription;
end;

{ TJsonFieldEnum }

constructor TJsonFieldEnum.Create;
begin
  inherited Create;
  fJsonEnumItems := TObjectList<TJsonFieldEnumItem>.Create(True);
  fEnumType := etNumber;
end;

destructor TJsonFieldEnum.Destroy;
begin
  FreeAndNil(fJsonEnumItems);
  inherited Destroy;
end;

procedure TJsonFieldEnum.AddItem(const pValue: Byte; const pDescription: string);
begin
  fJsonEnumItems.Add(TJsonFieldEnumItem.Create(pValue, pDescription));
end;

procedure TJsonFieldEnum.AddItems(const pItemsDescriptions: array of string);
var
  vItemIndex: Integer;
begin
  for vItemIndex := 0 to Length(pItemsDescriptions) -1 do
    Self.AddItem(vItemIndex, pItemsDescriptions[vItemIndex]);
end;

function TJsonFieldEnum.Clone: TJsonField;
var
  vIndex: Integer;
begin
  Result := inherited Clone;
  TJsonFieldEnum(Result).EnumType := Self.EnumType;
  for vIndex := 0 to Self.GetCount - 1 do
  begin
    TJsonFieldEnum(Result).AddItem(
      TJsonFieldEnumItem(fJsonEnumItems[vIndex]).Value,
      TJsonFieldEnumItem(fJsonEnumItems[vIndex]).Description);
  end;
end;

function TJsonFieldEnum.GetCount: Integer;
begin
  Result := fJsonEnumItems.Count;
end;

function TJsonFieldEnum.GetItem(const pIndex: Byte): TJsonFieldEnumItem;
begin
  if (pIndex < GetCount) then
    Result := TJsonFieldEnumItem(fJsonEnumItems[pIndex])
  else
    Result := nil;
end;

function TJsonFieldEnum.GetTypeName: string;
begin
  case fEnumType of
    etNumber: Result := inherited GetTypeName;
    etString: Result := 'string';
  end;
end;

function TJsonFieldEnum.ToJsonSchema: TJsonObject;
var
  vJsonList: TJsonArray;
  vCount: Integer;
  vJsonEnumItem: TJsonFieldEnumItem;
  vHelpEnum: string;
begin
  vHelpEnum := 'Enum [';
  vJsonList := TJsonArray.Create;
  for vCount := 0 to GetCount - 1 do
  begin
    vJsonEnumItem := GetItem(vCount);
    if Assigned(vJsonEnumItem) then
    begin
      vHelpEnum := vHelpEnum + vJsonEnumItem.Value.ToString + '=' + vJsonEnumItem.Description + ', ';
      case fEnumType of
        etNumber: vJsonList.Add(vJsonEnumItem.Value);
        etString: vJsonList.Add(vJsonEnumItem.Description);
      end;
    end;
  end;

  if (fEnumType = etNumber) then
    fDescription := fDescription + ' ' + Copy(vHelpEnum, 1, Length(vHelpEnum) - 2) + ']';

  Result := inherited ToJsonSchema;
  Result.AddPair('enum', vJsonList);
end;

initialization
  RegisterClass(TJsonFieldEnum);

end.
