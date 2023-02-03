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

unit MVCFramework.DataSet.Utils;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  System.Generics.Collections,
  System.JSON,
  System.Rtti,
  JsonDataObjects,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient;

type
  TFieldNamePolicy = (fpLowerCase, fpUpperCase, fpAsIs);

  TDataSetHelper = class helper for TDataSet
  public
    procedure LoadFromTValue(const Value: TValue;
      const aNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase);
    function AsJSONArray(FieldNameCase: TMVCNameCase = ncLowerCase): string;
    function AsJDOJSONArray(FieldNameCase: TMVCNameCase = ncLowerCase)
      : TJDOJsonArray;
    function MetadataAsJSONObject(FieldNameCase: TMVCNameCase = ncLowerCase)
      : TJSONObject;
    function AsJSONArrayOfValues: TJDOJsonArray;
    function AsJSONArrayString: string; deprecated 'Use AsJSONArray';
    function AsJSONObject(FieldNameCase: TMVCNameCase = ncLowerCase;
      const IgnoredFields: TArray<string> = nil): string;
    function AsJSONObjectString: string; deprecated 'Use AsJSONObject';
    procedure LoadFromJSONObject(const JSONObject: TJSONObject;
      const FieldNameCase: TMVCNameCase); overload;
    procedure LoadFromJSONObject(const JSONObject: TJSONObject;
      const AIgnoredFields: TArray<string> = nil;
      const FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;

    procedure LoadFromJSONArray(AJSONArray: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;
    procedure LoadFromJSONArray(AJSONArray: TJSONArray;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;

    procedure LoadJSONArrayFromJSONObjectProperty(PropertyName: string;
      JSONObject: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;
    procedure LoadJSONArrayFromJSONObjectProperty(PropertyName: string;
      JSONObject: TJSONObject;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;

    procedure LoadJSONObjectFromJSONObjectProperty(PropertyName: string;
      JSONObject: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;
    procedure LoadJSONObjectFromJSONObjectProperty(PropertyName: string;
      JSONObject: TJSONObject;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;

    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>;
      FieldNameCase: TMVCNameCase = ncLowerCase); overload; deprecated;
    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      FieldNameCase: TMVCNameCase = ncLowerCase); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string); overload;
    procedure LoadFromJSONObjectString(const JSONObjectString: string;
      const IgnoredFields: TArray<string>;
      const FieldNameCase: TMVCNameCase = ncLowerCase); overload;
    // procedure LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string; const aPropertyName: string;
    // const FieldNameCase: TMVCNameCase = ncLowerCase);
    procedure AppendFromJSONArrayString(AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncLowerCase); overload;
    function AsObjectList<T: class, constructor>(CloseAfterScroll
      : boolean = false; OwnsObjects: boolean = true): TObjectList<T>;
    function AsObject<T: class, constructor>(CloseAfterScroll
      : boolean = false): T;

  end;

  TDataSetUtils = class sealed
  private
    class var CTX: TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure DataSetToObject(ADataSet: TDataSet; AObject: TObject);
    class procedure DataSetToObjectList<T: class, constructor>
      (ADataSet: TDataSet; AObjectList: TObjectList<T>;
      ACloseDataSetAfterScroll: boolean = true);
  end;

  [MVCNameCase(ncLowerCase)]
  TDataSetHolder = class
  private
    fDataSet: TDataSet;
    fMetadata: TMVCStringDictionary;
    fOwns: boolean;
    fDataSetSerializationType: TMVCDatasetSerializationType;
  public
    constructor Create(const ADataSet: TDataSet; const AOwns: boolean = false;
      const ADataSetSerializationType
      : TMVCDatasetSerializationType = TMVCDatasetSerializationType.
      dstAllRecords); virtual;
    destructor Destroy; override;
    function SerializationType: TMVCDatasetSerializationType;
    [MVCNameAs('data')]
    property Items: TDataSet read fDataSet;
    [MVCNameAs('meta')]
    property Metadata: TMVCStringDictionary read fMetadata;
  end deprecated 'Use function "ObjectDict(boolean)" instead';

  TMVCAPIBinder = class
  protected type
    TMVCAPIBinderItem = class
    private
      fRESTClient: IMVCRESTClient;
      fDataSet: TDataSet;
      fURI: string;
      fPrimaryKeyNAme: string;
      fLoading: boolean;
      procedure ShowError(const AResponse: IMVCRESTResponse);
    public
      constructor Create(const aRESTClient: IMVCRESTClient; const ADataSet: TDataSet;
        const aURI, aPrimaryKeyName: string);
      destructor Destroy; override;
      procedure HookBeforePost(DataSet: TDataSet);
      procedure HookBeforeDelete(DataSet: TDataSet);
      procedure HookBeforeRefresh(DataSet: TDataSet);
      procedure HookAfterOpen(DataSet: TDataSet);
      // procedure HookBeforeRowRequest(DataSet: TFDDataSet);
    end;
  private
    fRESTClient: IMVCRESTClient;

  protected
    fItems: TObjectList<TMVCAPIBinderItem>;
  public
    constructor Create(const aRESTClient: IMVCRESTClient);
    destructor Destroy; override;
    procedure BindDataSetToAPI(const ADataSet: TDataSet; const aURI: string;
      const aPrimaryKeyName: string);
  end;

implementation

uses
  System.TypInfo,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Intf;

{ TDataSetHelper }

procedure TDataSetHelper.LoadFromTValue(const Value: TValue;
  const aNameCase: TMVCNameCase);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  if not({$IFDEF TOKYOORBETTER}Value.IsObjectInstance and
{$ENDIF} (Value.AsObject is TJDOJsonArray)) then
    raise Exception.Create
      ('LoadFromTValue requires a TValue containing a TJDOJsonArray');

  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lSer.JsonArrayToDataSet(TJSONArray(Value.AsObject), Self, [],
      TMVCNameCase.ncLowerCase);
  finally
    lSer.Free;
  end;

end;

// procedure TDataSetHelper.LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string;
// const aPropertyName: string; const FieldNameCase: TMVCNameCase);
// var
// lJson: TJSONObject;
// begin
// lJson := TJSONObject.Create;
// try
// lJson.FromJSON(AJSONObjectString);
// LoadFromJSONArray(lJson.A[aPropertyName], FieldNameCase);
// finally
// lJson.Free;
// end;
// end;

function TDataSetHelper.AsJDOJSONArray(FieldNameCase
  : TMVCNameCase = ncLowerCase): TJDOJsonArray;
var
  lSerializer: TMVCJsonDataObjectsSerializer;
begin
  Result := TJDOJsonArray.Create;
  try
    if not Eof then
    begin
      lSerializer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSerializer.DataSetToJsonArray(Self, Result, FieldNameCase, []);
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

function TDataSetHelper.AsJSONArray(FieldNameCase
  : TMVCNameCase = ncLowerCase): string;
var
  lSerializer: IMVCSerializer;
begin
  Result := '[]';
  if not Eof then
  begin
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    Result := lSerializer.SerializeDataSet(Self, [], FieldNameCase);
  end;
end;

function TDataSetHelper.AsJSONArrayString: string;
begin
  Result := AsJSONArray;
end;

function TDataSetHelper.AsJSONObject(FieldNameCase: TMVCNameCase;
  const IgnoredFields: TArray<string>): string;
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  Result := lSerializer.SerializeDataSetRecord(Self,
    TMVCIgnoredList(IgnoredFields), FieldNameCase);
end;

function TDataSetHelper.AsJSONObjectString: string;
begin
  Result := AsJSONObject(ncLowerCase);
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

function TDataSetHelper.AsObjectList<T>(CloseAfterScroll: boolean;
  OwnsObjects: boolean): TObjectList<T>;
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

function TDataSetHelper.MetadataAsJSONObject(FieldNameCase: TMVCNameCase)
  : TJSONObject;
var
  I: Integer;
  lObj: TJSONObject;
  lJArr: TJSONArray;
begin

  Result := TJSONObject.Create;
  try
    lJArr := Result.A['fielddefs'];
    for I := 0 to FieldDefs.Count - 1 do
    begin
      lObj := lJArr.AddObject;
      lObj.S['fieldname'] := TMVCSerializerHelper.ApplyNameCase(FieldNameCase,
        FieldDefList[I].Name);
      lObj.S['displayname'] := FieldDefList[I].DisplayName;
      lObj.I['datatype'] := Ord(FieldDefList[I].DataType);
      lObj.I['size'] := FieldDefList[I].Size;
      lObj.I['precision'] := FieldDefList[I].Precision;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: string;
  FieldNameCase: TMVCNameCase);
var
  lSerializer: IMVCSerializer;
begin
  Self.DisableControls;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    lSerializer.DeserializeDataSet(AJSONArray, Self, nil, FieldNameCase);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string;
  AIgnoredFields: TArray<string>; FieldNameCase: TMVCNameCase);
begin
  AppendFromJSONArrayString(AJSONArrayString, AIgnoredFields, FieldNameCase);
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: TJSONArray;
  FieldNameCase: TMVCNameCase);
var
  lSerializer: IMVCSerializer;
  lBookmark: TArray<Byte>;
begin
  lBookmark := Self.Bookmark;
  Self.DisableControls;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    TMVCJsonDataObjectsSerializer(lSerializer).JsonArrayToDataSet(AJSONArray, Self, nil, FieldNameCase);
    if Self.BookmarkValid(lBookmark) then
      Self.GotoBookmark(lBookmark);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadJSONArrayFromJSONObjectProperty
  (PropertyName: string; JSONObject: TJSONObject; FieldNameCase: TMVCNameCase);
begin
  LoadFromJSONArray(JSONObject.A[PropertyName], FieldNameCase);
end;

procedure TDataSetHelper.LoadJSONObjectFromJSONObjectProperty
  (PropertyName: string; JSONObject: TJSONObject; FieldNameCase: TMVCNameCase);
begin
  LoadFromJSONObject(JSONObject.O[PropertyName], FieldNameCase);
end;

procedure TDataSetHelper.LoadJSONObjectFromJSONObjectProperty(PropertyName,
  JSONObject: string; FieldNameCase: TMVCNameCase);
var
  lJObj: TJSONObject;
begin
  lJObj := StrToJSONObject(JSONObject);
  try
    LoadJSONObjectFromJSONObjectProperty(PropertyName, lJObj, FieldNameCase);
  finally
    lJObj.Free;
  end;
end;

procedure TDataSetHelper.LoadJSONArrayFromJSONObjectProperty(PropertyName,
  JSONObject: string; FieldNameCase: TMVCNameCase);
var
  lJson: TJSONObject;
begin
  lJson := StrToJSONObject(JSONObject);
  try
    LoadJSONArrayFromJSONObjectProperty(PropertyName, lJson, FieldNameCase);
  finally
    lJson.Free;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string;
  FieldNameCase: TMVCNameCase);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create(),
    FieldNameCase);
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string;
  AIgnoredFields: TArray<string>; FieldNameCase: TMVCNameCase);
begin
  LoadFromJSONArray(AJSONArrayString, FieldNameCase);
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create());
end;

procedure TDataSetHelper.LoadFromJSONObject(const JSONObject: TJSONObject;
  const AIgnoredFields: TArray<string>; const FieldNameCase: TMVCNameCase);
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  TMVCJsonDataObjectsSerializer(lSerializer).JsonObjectToDataSet(JSONObject, Self,
      TMVCIgnoredList(AIgnoredFields), FieldNameCase);
end;

procedure TDataSetHelper.LoadFromJSONObjectString(const JSONObjectString
  : string; const IgnoredFields: TArray<string>;
  const FieldNameCase: TMVCNameCase);
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  lSerializer.DeserializeDataSetRecord(JSONObjectString, Self,
    TMVCIgnoredList(IgnoredFields), FieldNameCase);
end;

procedure TDataSetHelper.LoadFromJSONObject(const JSONObject: TJSONObject;
  const FieldNameCase: TMVCNameCase);
begin
  LoadFromJSONObject(JSONObject, TArray<string>.Create(), FieldNameCase);
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

class procedure TDataSetUtils.DataSetToObject(ADataSet: TDataSet;
  AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  lRttiProp: TRttiProperty;
  _attribute: TCustomAttribute;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MVCColumnAttribute;
  field_name: string;
  Value: TValue;
  lNeedToSet, FoundAttribute: boolean;
  FoundTransientAttribute: boolean;
  LField: TField;
begin
  _dict := TDictionary<string, string>.Create();
  _keys := TDictionary<string, boolean>.Create();
  _type := CTX.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for lRttiProp in _fields do
  begin
    FoundAttribute := false;
    FoundTransientAttribute := false;
    for _attribute in lRttiProp.GetAttributes do
    begin
      if _attribute is MVCColumnAttribute then
      begin
        FoundAttribute := true;
        mf := MVCColumnAttribute(_attribute);
        _dict.Add(lRttiProp.Name, mf.FieldName);
        _keys.Add(lRttiProp.Name, mf.IsPK);
      end
      else if _attribute is MVCDoNotSerializeAttribute then
        FoundTransientAttribute := true;
    end;
    if ((not FoundAttribute) and (not FoundTransientAttribute)) then
    begin
      _dict.Add(lRttiProp.Name, lRttiProp.Name);
      _keys.Add(lRttiProp.Name, false);
    end;
  end;
  for lRttiProp in _fields do
  begin
    if not _dict.TryGetValue(lRttiProp.Name, field_name) then
      Continue;
    LField := ADataSet.FindField(field_name);
    if not Assigned(LField) then
      Continue;
    lNeedToSet := true;
    case lRttiProp.PropertyType.TypeKind of
      tkEnumeration: // tristan
        begin
          if lRttiProp.PropertyType.Handle = TypeInfo(boolean) then
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
      tkRecord:
        begin
          MapDataSetFieldToNullableRTTIProperty(lRttiProp.GetValue(AObject),
            LField, lRttiProp, AObject);
          lNeedToSet := false;
        end
    else
      Continue;
    end;
    if lNeedToSet then
    begin
      lRttiProp.SetValue(AObject, Value);
    end;
  end;
  _dict.Free;
  _keys.Free;
end;

class procedure TDataSetUtils.DataSetToObjectList<T>(ADataSet: TDataSet;
  AObjectList: TObjectList<T>; ACloseDataSetAfterScroll: boolean);
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

constructor TDataSetHolder.Create(const ADataSet: TDataSet;
  const AOwns: boolean = false; const ADataSetSerializationType
  : TMVCDatasetSerializationType = TMVCDatasetSerializationType.dstAllRecords);
begin
  inherited Create;
  fDataSet := ADataSet;
  fMetadata := TMVCStringDictionary.Create;
  fOwns := AOwns;
  fDataSetSerializationType := ADataSetSerializationType;
end;

destructor TDataSetHolder.Destroy;
begin
  fMetadata.Free;
  if fOwns then
  begin
    fDataSet.Free;
  end;
  inherited;
end;

function TDataSetHolder.SerializationType: TMVCDatasetSerializationType;
begin
  Result := fDataSetSerializationType;
end;

{ TMVCAPIBinder }

procedure TMVCAPIBinder.BindDataSetToAPI(const ADataSet: TDataSet;
  const aURI, aPrimaryKeyName: string);
begin
  fItems.Add(TMVCAPIBinderItem.Create(fRESTClient, ADataSet, aURI,
    aPrimaryKeyName));
end;

constructor TMVCAPIBinder.Create(const aRESTClient: IMVCRESTClient);
begin
  inherited Create;
  fItems := TObjectList<TMVCAPIBinderItem>.Create(true);
  fRESTClient := aRESTClient;
end;

destructor TMVCAPIBinder.Destroy;
begin
  fItems.Free;
  inherited;
end;

{ TMVCAPIBinder.TMVCAPIBinderItem }

constructor TMVCAPIBinder.TMVCAPIBinderItem.Create(const aRESTClient: IMVCRESTClient; const ADataSet: TDataSet;
  const aURI, aPrimaryKeyName: string);
begin
  inherited Create;
  fRESTClient := aRESTClient;
  fDataSet := ADataSet;
  fURI := aURI;
  fPrimaryKeyNAme := aPrimaryKeyName;

  fDataSet.BeforePost := HookBeforePost;
  fDataSet.BeforeDelete := HookBeforeDelete;
  fDataSet.BeforeRefresh := HookBeforeRefresh;
  fDataSet.AfterOpen := HookAfterOpen;
end;

destructor TMVCAPIBinder.TMVCAPIBinderItem.Destroy;
begin

  inherited;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.HookAfterOpen(DataSet: TDataSet);
var
  Res: IMVCRESTResponse;
  lData: TJSONObject;
begin

  // this a simple sychronous request...
  Res := fRESTClient.Get(fURI);
  if not Res.Success then
  begin
    ShowError(Res);
  end;

  lData := StrToJSONObject(Res.Content);
  try
    DataSet.DisableControls;
    try
      fLoading := true;
      DataSet.LoadFromJSONArray(lData.A['data']);
      fLoading := false;
      DataSet.First;
    finally
      DataSet.EnableControls;
    end;
  finally
    lData.Free;
  end;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.HookBeforeDelete(DataSet: TDataSet);
var
  Res: IMVCRESTResponse;
begin
  if DataSet.State = dsBrowse then
    Res := fRESTClient.DataSetDelete(fURI, DataSet.FieldByName(fPrimaryKeyNAme)
      .AsString);
  if not(Res.StatusCode in [200]) then
  begin
    ShowError(Res);
  end;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.HookBeforePost(DataSet: TDataSet);
var
  lRes: IMVCRESTResponse;
  lLastID: Integer;
begin
  if not fLoading then
  begin
    lLastID := -1;
    if fDataSet.State = dsInsert then
    begin
      lRes := fRESTClient.DataSetInsert(fURI, DataSet)
    end
    else
    begin
      lLastID := fDataSet.FieldByName(fPrimaryKeyNAme).AsInteger;
      lRes := fRESTClient.DataSetUpdate(fURI, lLastID.ToString, DataSet);
    end;
    if not(lRes.StatusCode in [200, 201]) then
    begin
      ShowError(lRes);
    end
    else
    begin
      DataSet.Refresh;
      if lLastID > -1 then
      begin
        DataSet.Locate('id', lLastID, []);
      end;
    end;
  end;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.HookBeforeRefresh(DataSet: TDataSet);
begin
  DataSet.Close;
  DataSet.Open;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.ShowError(const AResponse: IMVCRESTResponse);
begin
  if not AResponse.Success then
    raise EMVCException.Create(
      AResponse.StatusCode.ToString + ': ' + AResponse.StatusText + sLineBreak + AResponse.Content)
  else
    raise EMVCException.Create(AResponse.Content);
end;

end.
