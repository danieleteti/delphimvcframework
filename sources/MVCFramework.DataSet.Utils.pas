// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Serializer.Commons,
  MVCFramework.RESTClient;

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
    procedure LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string; const aPropertyName: string;
      const AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase);
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
    fDataSet: TDataSet;
    fMetadata: TMVCStringDictionary;
    fOwns: boolean;
    fDataSetSerializationType: TMVCDatasetSerializationType;
  public
    constructor Create(const ADataSet: TDataSet; const AOwns: boolean = false;
      const ADataSetSerializationType: TMVCDatasetSerializationType = TMVCDatasetSerializationType.
      dstAllRecords); virtual;
    destructor Destroy; override;
    function SerializationType: TMVCDatasetSerializationType;
    [MVCNameAs('data')]
    property Items: TDataSet read fDataSet;
    [MVCNameAs('meta')]
    property Metadata: TMVCStringDictionary read fMetadata;
  end;

  TMVCAPIBinder = class
  protected
    type
    TMVCAPIBinderItem = class
    private
      fRESTClient: TRESTClient;
      fDataSet: TDataSet;
      fURI: string;
      fPrimaryKeyNAme: string;
      fLoading: boolean;
      procedure ShowError(const AResponse: IRESTResponse);
    public
      constructor Create(const aRESTClient: TRESTClient; const ADataSet: TDataSet;
        const aURI, aPrimaryKeyName: string);
      destructor Destroy; override;
      procedure HookBeforePost(DataSet: TDataSet);
      procedure HookBeforeDelete(DataSet: TDataSet);
      procedure HookBeforeRefresh(DataSet: TDataSet);
      procedure HookAfterOpen(DataSet: TDataSet);
      // procedure HookBeforeRowRequest(DataSet: TFDDataSet);
    end;
  private
    fRESTClient: TRESTClient;

  protected
    fItems: TObjectList<TMVCAPIBinderItem>;
  public
    constructor Create(const aRESTClient: TRESTClient);
    destructor Destroy; override;
    procedure BindDataSetToAPI(const ADataSet: TDataSet; const aURI: string; const aPrimaryKeyName: string);
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
    AMetaFiller(Result.fMetadata);
  end;
end;

function NewDataSetHolder(const ADataSet: TDataSet; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataSetHolder;
begin
  Result := TDataSetHolder.Create(ADataSet, AOwns, dstAllRecords);
  if Assigned(AMetaFiller) then
  begin
    AMetaFiller(Result.fMetadata);
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

procedure TDataSetHelper.LoadJSONArrayFromJSONObjectProperty(const AJSONObjectString: string;
  const aPropertyName: string; const AFieldNamePolicy: TFieldNamePolicy);
var
  lJson: TJSONObject;
begin
  lJson := TJSONObject.Create;
  try
    lJson.FromJSON(AJSONObjectString);
    LoadFromJSONArray(lJson.A[aPropertyName], AFieldNamePolicy);
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
          MapDataSetFieldToNullableRTTIProperty(lRttiProp.GetValue(AObject), LField, lRttiProp, AObject);
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

procedure TMVCAPIBinder.BindDataSetToAPI(const ADataSet: TDataSet; const aURI,
  aPrimaryKeyName: string);
begin
  fItems.Add(TMVCAPIBinderItem.Create(fRESTClient, ADataSet, aURI, aPrimaryKeyName));
end;

constructor TMVCAPIBinder.Create(const aRESTClient: TRESTClient);
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

constructor TMVCAPIBinder.TMVCAPIBinderItem.Create(const aRESTClient: TRESTClient; const ADataSet: TDataSet;
  const aURI, aPrimaryKeyName: string);
begin
  inherited Create;
  fRESTClient := aRESTClient;
  fDataSet := ADataSet;
  fURI := aURI;
  fPrimaryKeyNAme := aPrimaryKeyName;

  // procedure HookBeforePost(DataSet: TDataSet);
  // procedure HookBeforeDelete(DataSet: TDataSet);
  // procedure HookBeforeRefresh(DataSet: TDataSet);
  // procedure HookAfterOpen(DataSet: TDataSet);

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
  Res: IRESTResponse;
  lData: TJSONObject;
begin
  // this a simple sychronous request...
  Res := fRESTClient.doGET('/api/customers', []);
  if Res.HasError then
  begin
    ShowError(Res);
  end;

  lData := StrToJSONObject(Res.BodyAsString);
  try
    DataSet.DisableControls;
    try
      fLoading := true;
      DataSet.LoadFromJSONArray(lData.A['items']);
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
  Res: IRESTResponse;
begin
  if DataSet.State = dsBrowse then
    Res := fRESTClient.DataSetDelete(fURI, DataSet.FieldByName(fPrimaryKeyNAme).AsString);
  if not(Res.ResponseCode in [200]) then
  begin
    ShowError(Res);
  end;
end;

procedure TMVCAPIBinder.TMVCAPIBinderItem.HookBeforePost(DataSet: TDataSet);
var
  lRes: IRESTResponse;
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
      lRes := fRESTClient.DataSetUpdate(fURI, DataSet, lLastID.ToString);
    end;
    if not(lRes.ResponseCode in [200, 201]) then
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

procedure TMVCAPIBinder.TMVCAPIBinderItem.ShowError(const AResponse: IRESTResponse);
begin
  if AResponse.HasError then
    raise EMVCException.Create(AResponse.Error.Status);
  // else
  // MessageDlg(
  // AResponse.ResponseCode.ToString + ': ' + AResponse.ResponseText + sLineBreak +
  // AResponse.BodyAsString,
  // mtError, [mbOK], 0);
end;

end.
