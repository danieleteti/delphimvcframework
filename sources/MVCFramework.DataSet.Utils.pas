// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.DataSet.Utils;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  Data.DB,
  System.Generics.Collections,
  System.JSON,
  System.Rtti,
  JsonDataObjects,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type
  TFieldNamePolicy = (fpLowerCase, fpUpperCase, fpAsIs);

  TDataSetHelper = class helper for TDataSet
  public
    procedure LoadFromTValue(const Value: TValue; const aNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase);
    function AsJSONArray: string;
    function AsJDOJSONArray: TJDOJsonArray;
    function AsJSONArrayOfValues: TJDOJsonArray;
    function AsJSONArrayString: string; deprecated 'Use AsJSONArray';
    function AsJSONObject(AFieldNamePolicy: TFieldNamePolicy = fpLowerCase): string;
    function AsJSONObjectString: string; deprecated 'Use AsJSONObject';
    procedure LoadFromJSONObject(AJSONObject: TJSONObject; AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    procedure LoadFromJSONObject(AJSONObject: TJSONObject; AIgnoredFields: TArray<string>;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    procedure LoadFromJSONArray(AJSONArray: string;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONArrayString(AJSONArrayString: string; AIgnoredFields: TArray<string>;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONArray(AJSONArray: TJSONArray;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string; AIgnoredFields: TArray<string>); overload;
    procedure LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string; const aPropertyName: String; const aFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase);
    procedure AppendFromJSONArrayString(AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString(AJSONArrayString: string; AIgnoredFields: TArray<string>;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    function AsObjectList<T: class, constructor>(CloseAfterScroll: boolean = false; OwnsObjects: boolean = true)
      : TObjectList<T>;
    function AsObject<T: class, constructor>(CloseAfterScroll: boolean = false): T;
  end;

  TDataSetUtils = class sealed
  private
    class var CTX: TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure DataSetToObject(ADataSet: TDataSet; AObject: TObject);
    class procedure DataSetToObjectList<T: class, constructor>(ADataSet: TDataSet; AObjectList: TObjectList<T>;
      ACloseDataSetAfterScroll: boolean = true);
  end;

  [MVCNameCase(ncLowerCase)]
  TDataSetHolder = class
  private
    FDataSet: TDataSet;
    FMetadata: TMVCStringDictionary;
    FOwns: boolean;
    FDataSetSerializationType: TMVCDatasetSerializationType;
  public
    constructor Create(const ADataSet: TDataSet; const AOwns: boolean = false;
      const ADataSetSerializationType: TMVCDatasetSerializationType = TMVCDatasetSerializationType.
      dstAllRecords); virtual;
    destructor Destroy; override;
    function SerializationType: TMVCDatasetSerializationType;
    [MVCNameAs('data')]
    property Items: TDataSet read FDataSet;
    [MVCNameAs('meta')]
    property Metadata: TMVCStringDictionary read FMetadata;
  end;

function NewDataSetHolder(const ADataSet: TDataSet; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataSetHolder;
function NewDataSetRecordHolder(const ADataSet: TDataSet; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataSetHolder;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Intf;

function NewDataSetRecordHolder(const ADataSet: TDataSet; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataSetHolder;
begin
  Result := TDataSetHolder.Create(ADataSet, AOwns, dstSingleRecord);
  if Assigned(AMetaFiller) then
  begin
    AMetaFiller(Result.FMetadata);
  end;
end;

function NewDataSetHolder(const ADataSet: TDataSet; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataSetHolder;
begin
  Result := TDataSetHolder.Create(ADataSet, AOwns, dstAllRecords);
  if Assigned(AMetaFiller) then
  begin
    AMetaFiller(Result.FMetadata);
  end;
end;

{ TDataSetHelper }

procedure TDataSetHelper.LoadFromTValue(const Value: TValue; const aNameCase: TMVCNameCase);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  if not({$IFDEF TOKYOORBETTER}Value.IsObjectInstance and
{$ENDIF} (Value.AsObject is TJDOJsonArray)) then
    raise Exception.Create('LoadFromTValue requires a TValue containing a TJDOJsonArray');

  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lSer.JsonArrayToDataSet(TJSONArray(Value.AsObject), Self, [], TMVCNameCase.ncLowerCase);
  finally
    lSer.Free;
  end;

end;

procedure TDataSetHelper.LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string; const aPropertyName: String; const aFieldNamePolicy: TFieldNamePolicy);
var
  lJson: TJSONObject;
begin
  lJson := TJSONObject.Create;
  try
    lJson.FromJSON(AJSONObjectString);
    LoadFromJSONArray(lJson.A[aPropertyName], aFieldNamePolicy);
  finally
    lJson.Free;
  end;
end;

function TDataSetHelper.AsJDOJSONArray: TJDOJsonArray;
var
  lSerializer: TMVCJsonDataObjectsSerializer;
begin
  Result := TJDOJsonArray.Create;
  try
    if not Eof then
    begin
      lSerializer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSerializer.DataSetToJsonArray(Self, Result, ncLowerCase, []);
      finally
        lSerializer.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TDataSetHelper.AsJSONArrayOfValues: TJDOJsonArray;
var
  lSerializer: TMVCJsonDataObjectsSerializer;
begin
  Result := TJDOJsonArray.Create;
  try
    if not Eof then
    begin
      lSerializer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSerializer.DataSetToJsonArrayOfValues(Self, Result, []);
      finally
        lSerializer.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TDataSetHelper.AsJSONArray: string;
var
  lSerializer: IMVCSerializer;
begin
  Result := '[]';
  if not Eof then
  begin
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    Result := lSerializer.SerializeDataSet(Self, [], ncLowerCase);
  end;
end;

function TDataSetHelper.AsJSONArrayString: string;
begin
  Result := AsJSONArray;
end;

function TDataSetHelper.AsJSONObject(AFieldNamePolicy: TFieldNamePolicy): string;
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  Result := lSerializer.SerializeDataSetRecord(Self, [], ncAsIs);
  // Mapper.DataSetToJSONObject(Self, JObj, false);
end;

function TDataSetHelper.AsJSONObjectString: string;
begin
  Result := AsJSONObject(fpLowerCase);
end;

function TDataSetHelper.AsObject<T>(CloseAfterScroll: boolean): T;
var
  Obj: T;
begin
  if not Self.Eof then
  begin
    Obj := T.Create;
    try
      TDataSetUtils.DataSetToObject(Self, Obj);
      Result := Obj;
    except
      FreeAndNil(Obj);
      raise;
    end;
  end
  else
    Result := nil;
end;

function TDataSetHelper.AsObjectList<T>(CloseAfterScroll: boolean; OwnsObjects: boolean): TObjectList<T>;
var
  lObjs: TObjectList<T>;
begin
  lObjs := TObjectList<T>.Create(OwnsObjects);
  try
    TDataSetUtils.DataSetToObjectList<T>(Self, lObjs, CloseAfterScroll);
    Result := lObjs;
  except
    FreeAndNil(lObjs);
    raise;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: string; AFieldNamePolicy: TFieldNamePolicy);
var
  lSerializer: IMVCSerializer;
begin
  Self.DisableControls;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    lSerializer.DeserializeDataSet(AJSONArray, Self, nil, ncAsIs);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string; AIgnoredFields: TArray<string>;
  AFieldNamePolicy: TFieldNamePolicy);
begin
  AppendFromJSONArrayString(AJSONArrayString, AIgnoredFields, AFieldNamePolicy);
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: TJSONArray; AFieldNamePolicy: TFieldNamePolicy);
var
  lSerializer: TMVCJsonDataObjectsSerializer;
  lBookmark: TArray<Byte>;
begin
  lBookmark := Self.Bookmark;
  Self.DisableControls;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    try
      lSerializer.JsonArrayToDataSet(AJSONArray, Self, nil, ncAsIs);
    finally
      lSerializer.Free;
    end;
    if Self.BookmarkValid(lBookmark) then
      Self.GotoBookmark(lBookmark);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string; AFieldNamePolicy: TFieldNamePolicy);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create(), AFieldNamePolicy);
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string; AIgnoredFields: TArray<string>;
  AFieldNamePolicy: TFieldNamePolicy);
begin
  LoadFromJSONArray(AJSONArrayString, AFieldNamePolicy);
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create());
end;

procedure TDataSetHelper.LoadFromJSONObject(AJSONObject: TJSONObject; AIgnoredFields: TArray<string>;
  AFieldNamePolicy: TFieldNamePolicy);
begin
  raise Exception.Create('Not Implemented');
  // Mapper.JSONObjectToDataSet(AJSONObject, Self, AIgnoredFields, false,
  // AFieldNamePolicy);
end;

procedure TDataSetHelper.LoadFromJSONObjectString(AJSONObjectString: string; AIgnoredFields: TArray<string>);
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  lSerializer.DeserializeDataSetRecord(AJSONObjectString, Self, nil, ncAsIs);
end;

procedure TDataSetHelper.LoadFromJSONObject(AJSONObject: TJSONObject; AFieldNamePolicy: TFieldNamePolicy);
begin
  LoadFromJSONObject(AJSONObject, TArray<string>.Create());
end;

procedure TDataSetHelper.LoadFromJSONObjectString(AJSONObjectString: string);
begin
  LoadFromJSONObjectString(AJSONObjectString, TArray<string>.Create());
end;

{ TDataSetUtils }

class constructor TDataSetUtils.Create;
begin
  TDataSetUtils.CTX := TRttiContext.Create;
end;

class procedure TDataSetUtils.DataSetToObject(ADataSet: TDataSet; AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  _attribute: TCustomAttribute;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MVCColumnAttribute;
  field_name: string;
  Value: TValue;
  FoundAttribute: boolean;
  FoundTransientAttribute: boolean;
  LField: TField;
begin
  _dict := TDictionary<string, string>.Create();
  _keys := TDictionary<string, boolean>.Create();
  _type := CTX.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
  begin
    FoundAttribute := false;
    FoundTransientAttribute := false;
    for _attribute in _field.GetAttributes do
    begin
      if _attribute is MVCColumnAttribute then
      begin
        FoundAttribute := true;
        mf := MVCColumnAttribute(_attribute);
        _dict.Add(_field.Name, mf.FieldName);
        _keys.Add(_field.Name, mf.IsPK);
      end
      else if _attribute is MVCDoNotSerializeAttribute then
        FoundTransientAttribute := true;
    end;
    if ((not FoundAttribute) and (not FoundTransientAttribute)) then
    begin
      _dict.Add(_field.Name, _field.Name);
      _keys.Add(_field.Name, false);
    end;
  end;
  for _field in _fields do
  begin

    if not _dict.TryGetValue(_field.Name, field_name) then
      Continue;

    LField := ADataSet.FindField(field_name);

    if not Assigned(LField) then
      Continue;

    case _field.PropertyType.TypeKind of
      tkEnumeration: // tristan
        begin
          if _field.PropertyType.Handle = TypeInfo(boolean) then
          begin
            case LField.DataType of
              ftInteger, ftSmallint, ftLargeint:
                begin
                  Value := (LField.AsInteger = 1);
                end;
              ftBoolean:
                begin
                  Value := LField.AsBoolean;
                end;
            else
              Continue;
            end;
          end;
        end;
      tkInteger:
        Value := LField.AsInteger;
      tkInt64:
        Value := LField.AsLargeInt;
      tkFloat:
        Value := LField.AsFloat;
      tkString:
        Value := LField.AsString;
      tkUString, tkWChar, tkLString, tkWString:
        Value := LField.AsWideString;
    else
      Continue;
    end;
    _field.SetValue(AObject, Value);
  end;
  _dict.Free;
  _keys.Free;
end;

class procedure TDataSetUtils.DataSetToObjectList<T>(ADataSet: TDataSet; AObjectList: TObjectList<T>;
  ACloseDataSetAfterScroll: boolean);
var
  Obj: T;
  SavedPosition: TArray<Byte>;
begin
  ADataSet.DisableControls;
  try
    SavedPosition := ADataSet.Bookmark;
    while not ADataSet.Eof do
    begin
      Obj := T.Create;
      DataSetToObject(ADataSet, Obj);
      AObjectList.Add(Obj);
      ADataSet.Next;
    end;
    if ADataSet.BookmarkValid(SavedPosition) then
      ADataSet.Bookmark := SavedPosition;
  finally
    ADataSet.EnableControls;
  end;
  if ACloseDataSetAfterScroll then
    ADataSet.Close;
end;

class destructor TDataSetUtils.Destroy;
begin
  CTX.Free;
end;

{ TDataSetHolder }

constructor TDataSetHolder.Create(const ADataSet: TDataSet; const AOwns: boolean = false;
  const ADataSetSerializationType: TMVCDatasetSerializationType = TMVCDatasetSerializationType.dstAllRecords);
begin
  inherited Create;
  FDataSet := ADataSet;
  FMetadata := TMVCStringDictionary.Create;
  FOwns := AOwns;
  FDataSetSerializationType := ADataSetSerializationType;
end;

destructor TDataSetHolder.Destroy;
begin
  FMetadata.Free;
  if FOwns then
  begin
    FDataSet.Free;
  end;
  inherited;
end;

function TDataSetHolder.SerializationType: TMVCDatasetSerializationType;
begin
  Result := FDataSetSerializationType;
end;

end.
