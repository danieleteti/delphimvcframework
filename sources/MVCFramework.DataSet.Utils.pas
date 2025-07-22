// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.RESTClient, MVCFramework.JSONRPC;

type
  TFieldNamePolicy = (fpLowerCase, fpUpperCase, fpAsIs);

  TMVCCSVExportSettings = record
    IncludeHeaders: Boolean;
    Delimiter: Char;
    QuoteChar: Char;
    EscapeQuotes: Boolean;
    LineEnding: string;
    DateFormat: string;
    DateTimeFormat: string;
    TimeFormat: string;
    DecimalSeparator: Char;
    BooleanTrueValue: string;
    BooleanFalseValue: string;
    NullValueRepresentation: string;
    AlwaysQuoteStrings: Boolean;
    QuoteEmptyStrings: Boolean;
    TrimStringValues: Boolean;

    class function Default: TMVCCSVExportSettings; static;
    class function Excel: TMVCCSVExportSettings; static;
    class function RFC4180: TMVCCSVExportSettings; static;
  end;

  EMVCCSVSerializationError = class(EMVCSerializationException);

  TMVCDataSetCSVSerializer = class
  private
    FSettings: TMVCCSVExportSettings;
    function EscapeCSVValue(const AValue: string): string;
    function FormatFieldValue(AField: TField): string;
    function ShouldQuoteValue(const AValue: string): Boolean;
    function GetFieldHeaders(ADataSet: TDataSet): string;
    function GetDataRow(ADataSet: TDataSet): string;
  public
    constructor Create(const ASettings: TMVCCSVExportSettings);

    function SerializeToString(ADataSet: TDataSet): string;
    procedure SerializeToStream(ADataSet: TDataSet; AStream: TStream;
      AEncoding: TEncoding = nil);
    procedure SerializeToFile(ADataSet: TDataSet; const AFileName: string;
      AEncoding: TEncoding = nil);
    property Settings: TMVCCSVExportSettings read FSettings write FSettings;
  end;

  TDataSetHelper = class helper for TDataSet
  public
    procedure LoadFromJSONRPCResponse(const Value: IJSONRPCResponse; const aNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault);
    procedure LoadFromTValue(const Value: TValue;
      const aNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault);
    function AsJSONArray(FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault): string;
    function AsJDOJSONArray(FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault)
      : TJDOJsonArray;
    function MetadataAsJSONObject(FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault)
      : TJSONObject;
    function AsJSONArrayOfValues: TJDOJsonArray;
    function AsJSONObject(FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault;
      const IgnoredFields: TArray<string> = nil): string;
    function AsJDOJSONObject(FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault; const IgnoredFields: TArray<string> = nil): TJDOJsonObject;
    procedure LoadFromJSONObject(const JSONObject: TJSONObject;
      const FieldNameCase: TMVCNameCase); overload;
    procedure LoadFromJSONObject(const JSONObject: TJSONObject;
      const AIgnoredFields: TArray<string> = nil;
      const FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;

    procedure LoadFromJSONArray(AJSONArray: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;
    procedure LoadFromJSONArray(AJSONArray: TJSONArray;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;

    procedure LoadJSONArrayFromJSONObjectProperty(PropertyName: string;
      JSONObject: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;
    procedure LoadJSONArrayFromJSONObjectProperty(PropertyName: string;
      JSONObject: TJSONObject;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;

    procedure LoadJSONObjectFromJSONObjectProperty(PropertyName: string;
      JSONObject: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;
    procedure LoadJSONObjectFromJSONObjectProperty(PropertyName: string;
      JSONObject: TJSONObject;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;

    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload; deprecated;
    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string); overload;
    procedure LoadFromJSONObjectString(const JSONObjectString: string;
      const IgnoredFields: TArray<string>;
      const FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;

    procedure AppendFromJSONArrayString(AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>;
      FieldNameCase: TMVCNameCase = TMVCNameCase.ncUseDefault); overload;
    function AsObjectList<T: class, constructor>(CloseAfterScroll: boolean = false; OwnsObjects: Boolean = True): TObjectList<T>;
    function AsObject<T: class, constructor>(CloseAfterScroll: boolean = false): T;

    // CSV Oriented methods
    function AsCSV(const CSVExportSettings: TMVCCSVExportSettings; const Encoding: TEncoding = nil): String;
    procedure AsCSVStream(const CSVExportSettings: TMVCCSVExportSettings; const Stream: TStream; const Encoding: TEncoding = nil);
    procedure AsCSVFile(const CSVExportSettings: TMVCCSVExportSettings; const FileName: String; const Encoding: TEncoding = nil);
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

  [MVCNameCase(ncUseDefault)]
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
  MVCFramework.ActiveRecord,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Intf;

{ TDataSetHelper }

procedure TDataSetHelper.LoadFromTValue(const Value: TValue;
  const aNameCase: TMVCNameCase);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  if not({$IFDEF TOKYOORBETTER}Value.IsObjectInstance and{$ENDIF} (Value.AsObject is TJDOJsonArray)) then
    raise Exception.Create('LoadFromTValue requires a TValue containing a TJDOJsonArray');

  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    DisableControls;
    try
      lSer.JsonArrayToDataSet(TJSONArray(Value.AsObject), Self, [],
        TMVCNameCase.ncUseDefault);
      First;
    finally
      EnableControls;
    end;
  finally
    lSer.Free;
  end;

end;

function TDataSetHelper.AsCSV(const CSVExportSettings: TMVCCSVExportSettings; const Encoding: TEncoding): String;
var
  lSer: TMVCDataSetCSVSerializer;
begin
  lSer := TMVCDataSetCSVSerializer.Create(CSVExportSettings);
  try
    Result := lSer.SerializeToString(Self);
  finally
    lSer.Free;
  end;
end;

procedure TDataSetHelper.AsCSVFile(const CSVExportSettings: TMVCCSVExportSettings; const FileName: String;
  const Encoding: TEncoding);
var
  lSer: TMVCDataSetCSVSerializer;
begin
  lSer := TMVCDataSetCSVSerializer.Create(CSVExportSettings);
  try
    lSer.SerializeToFile(Self, FileName, Encoding);
  finally
    lSer.Free;
  end;
end;

procedure TDataSetHelper.AsCSVStream(const CSVExportSettings: TMVCCSVExportSettings; const Stream: TStream;
  const Encoding: TEncoding);
var
  lSer: TMVCDataSetCSVSerializer;
begin
  lSer := TMVCDataSetCSVSerializer.Create(CSVExportSettings);
  try
    lSer.SerializeToStream(Self, Stream, Encoding);
  finally
    lSer.Free;
  end;
end;

function TDataSetHelper.AsJDOJSONArray(FieldNameCase
  : TMVCNameCase = TMVCNameCase.ncUseDefault): TJDOJsonArray;
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

function TDataSetHelper.AsJDOJSONObject(FieldNameCase: TMVCNameCase; const IgnoredFields: TArray<string>): TJDOJsonObject;
var
  lSerializer: TMVCJsonDataObjectsSerializer;
  lDSFields: TMVCDataSetFields;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  try
    Result := TJDOJsonObject.Create;
    try
      lDSFields := lSerializer.GetDataSetFields(Self, TMVCIgnoredList(IgnoredFields), FieldNameCase);
      try
        lSerializer.DataSetToJsonObject(Self, Result, FieldNameCase, TMVCIgnoredList(IgnoredFields), lDSFields);
      finally
        lDSFields.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    lSerializer.Free;
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
  : TMVCNameCase = TMVCNameCase.ncUseDefault): string;
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

function TDataSetHelper.AsJSONObject(FieldNameCase: TMVCNameCase;
  const IgnoredFields: TArray<string>): string;
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  Result := lSerializer.SerializeDataSetRecord(Self,
    TMVCIgnoredList(IgnoredFields), FieldNameCase);
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

procedure TDataSetHelper.LoadFromJSONRPCResponse(const Value: IJSONRPCResponse; const aNameCase: TMVCNameCase);
begin
  LoadFromTValue(Value.Result, aNameCase);
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
  tf: MVCTableFieldAttribute;
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
      else if _attribute is MVCTableFieldAttribute then
      begin
        FoundAttribute := true;
        tf := MVCTableFieldAttribute(_attribute);
        _dict.Add(lRttiProp.Name, tf.FieldName);
        _keys.Add(lRttiProp.Name, foPrimaryKey in tf.FieldOptions);
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


{ TMVCCSVExportSettings }

class function TMVCCSVExportSettings.Default: TMVCCSVExportSettings;
begin
  Result.IncludeHeaders := True;
  Result.Delimiter := ',';
  Result.QuoteChar := '"';
  Result.EscapeQuotes := True;
  Result.LineEnding := sLineBreak;
  Result.DateFormat := 'yyyy-mm-dd';
  Result.DateTimeFormat := 'yyyy-mm-dd hh:nn:ss';
  Result.TimeFormat := 'hh:nn:ss';
  Result.DecimalSeparator := '.';
  Result.BooleanTrueValue := 'True';
  Result.BooleanFalseValue := 'False';
  Result.NullValueRepresentation := '';
  Result.AlwaysQuoteStrings := False;
  Result.QuoteEmptyStrings := False;
  Result.TrimStringValues := True;
end;

class function TMVCCSVExportSettings.Excel: TMVCCSVExportSettings;
begin
  Result := Default;
  Result.Delimiter := ';';
  Result.DecimalSeparator := ',';
  Result.AlwaysQuoteStrings := True;
end;

class function TMVCCSVExportSettings.RFC4180: TMVCCSVExportSettings;
begin
  Result := Default;
  Result.LineEnding := #13#10;
  Result.AlwaysQuoteStrings := False;
  Result.QuoteEmptyStrings := False;
end;

{ TDataSetCSVSerializer }

constructor TMVCDataSetCSVSerializer.Create(const ASettings: TMVCCSVExportSettings);
begin
  inherited Create;
  FSettings := ASettings;
end;

function TMVCDataSetCSVSerializer.EscapeCSVValue(const AValue: string): string;
var
  NeedsQuoting: Boolean;
begin
  Result := AValue;

  // Determina se il valore necessita di quoting
  NeedsQuoting := ShouldQuoteValue(AValue);

  if NeedsQuoting then
  begin
    // Escape delle virgolette interne (raddoppiamento secondo RFC 4180)
    if FSettings.EscapeQuotes and (Pos(FSettings.QuoteChar, Result) > 0) then
      Result := StringReplace(Result, FSettings.QuoteChar,
                             FSettings.QuoteChar + FSettings.QuoteChar, [rfReplaceAll]);

    // Racchiudi tra virgolette
    Result := FSettings.QuoteChar + Result + FSettings.QuoteChar;
  end;
end;

function TMVCDataSetCSVSerializer.ShouldQuoteValue(const AValue: string): Boolean;
begin
  Result := FSettings.AlwaysQuoteStrings or
           (FSettings.QuoteEmptyStrings and (AValue = '')) or
           (Pos(FSettings.Delimiter, AValue) > 0) or
           (Pos(FSettings.QuoteChar, AValue) > 0) or
           (Pos(#13, AValue) > 0) or
           (Pos(#10, AValue) > 0);
end;

function TMVCDataSetCSVSerializer.FormatFieldValue(AField: TField): string;
var
  lFormatSettings: TFormatSettings;
begin
  if AField.IsNull then
  begin
    Result := FSettings.NullValueRepresentation;
    Exit;
  end;

  case AField.DataType of
    ftString, ftMemo, ftWideMemo, ftWideString, ftFixedChar, ftFixedWideChar:
      begin
        Result := AField.AsString;
        if FSettings.TrimStringValues then
          Result := Trim(Result);
        Result := EscapeCSVValue(Result);
      end;

    ftInteger, ftLargeint, ftAutoInc, ftSmallint, ftWord, ftLongWord:
      Result := AField.AsString;

    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      begin
        lFormatSettings:= TFormatSettings.Create('us_US');
        lFormatSettings.DecimalSeparator := FSettings.DecimalSeparator;
        Result := FloatToStr(AField.AsExtended, lFormatSettings);
      end;

    ftDate:
      Result := FormatDateTime(FSettings.DateFormat, AField.AsDateTime);

    ftTime:
      Result := FormatDateTime(FSettings.TimeFormat, AField.AsDateTime);

    ftDateTime, ftTimeStamp:
      Result := FormatDateTime(FSettings.DateTimeFormat, AField.AsDateTime);

    ftBoolean:
      if AField.AsBoolean then
        Result := FSettings.BooleanTrueValue
      else
        Result := FSettings.BooleanFalseValue;

    ftBlob, ftGraphic, ftOraBlob, ftOraClob:
      begin
        Result := EscapeCSVValue('[BLOB Data]');
      end;

    else
      Result := EscapeCSVValue(AField.AsString);
  end;
end;

function TMVCDataSetCSVSerializer.GetFieldHeaders(ADataSet: TDataSet): string;
var
  I: Integer;
  HeaderList: TStringList;
begin
  HeaderList := TStringList.Create;
  try
    HeaderList.Delimiter := FSettings.Delimiter;
    HeaderList.StrictDelimiter := True;
    HeaderList.QuoteChar := #0;


    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      if ADataSet.Fields[I].Visible then
      begin
        HeaderList.Add(EscapeCSVValue(ADataSet.Fields[I].FieldName));
      end;
    end;

    Result := HeaderList.DelimitedText;
  finally
    HeaderList.Free;
  end;
end;

function TMVCDataSetCSVSerializer.GetDataRow(ADataSet: TDataSet): string;
var
  I: Integer;
  RowData: TStringList;
begin
  RowData := TStringList.Create;
  try
    RowData.Delimiter := FSettings.Delimiter;
    RowData.StrictDelimiter := True;
    RowData.QuoteChar := #0;

    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      if ADataSet.Fields[I].Visible then
        RowData.Add(FormatFieldValue(ADataSet.Fields[I]));
    end;

    Result := RowData.DelimitedText;
  finally
    RowData.Free;
  end;
end;

function TMVCDataSetCSVSerializer.SerializeToString(ADataSet: TDataSet): string;
var
  Output: TStringBuilder;
  BookmarkSaved: TBookmark;
begin
  if not Assigned(ADataSet) then
    raise EMVCCSVSerializationError.Create('DataSet non assegnato');

  if not ADataSet.Active then
    raise EMVCCSVSerializationError.Create('DataSet non attivo');

  Output := TStringBuilder.Create;
  try
    BookmarkSaved := ADataSet.Bookmark;
    try
      ADataSet.DisableControls;
      try
        // Headers
        if FSettings.IncludeHeaders then
        begin
          Output.Append(GetFieldHeaders(ADataSet));
          Output.Append(FSettings.LineEnding);
        end;

        // Data rows
        ADataSet.First;
        while not ADataSet.Eof do
        begin
          Output.Append(GetDataRow(ADataSet));
          ADataSet.Next;

          if not ADataSet.Eof then
            Output.Append(FSettings.LineEnding);
        end;

      finally
        ADataSet.EnableControls;
      end;
    finally
      if ADataSet.BookmarkValid(BookmarkSaved) then
        ADataSet.Bookmark := BookmarkSaved;
    end;

    Result := Output.ToString;
  finally
    Output.Free;
  end;
end;

procedure TMVCDataSetCSVSerializer.SerializeToStream(ADataSet: TDataSet;
  AStream: TStream; AEncoding: TEncoding);
var
  CSVData: string;
  Bytes: TBytes;
begin
  if not Assigned(AStream) then
    raise EMVCCSVSerializationError.Create('Stream non assegnato');

  if not Assigned(AEncoding) then
    AEncoding := TEncoding.UTF8;

  CSVData := SerializeToString(ADataSet);
  Bytes := AEncoding.GetBytes(CSVData);
  AStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure TMVCDataSetCSVSerializer.SerializeToFile(ADataSet: TDataSet;
  const AFileName: string; AEncoding: TEncoding);
var
  FileStream: TFileStream;
begin
  if AFileName = '' then
    raise EMVCCSVSerializationError.Create('Nome file non specificato');

  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SerializeToStream(ADataSet, FileStream, AEncoding);
  finally
    FileStream.Free;
  end;
end;

end.
