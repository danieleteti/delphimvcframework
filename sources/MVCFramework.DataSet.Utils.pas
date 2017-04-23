unit MVCFramework.DataSet.Utils;

interface

uses System.SysUtils, Data.DB, System.Generics.Collections, System.JSON,
  System.Rtti, JsonDataObjects;

type
  TFieldNamePolicy = (fpLowerCase, fpUpperCase, fpAsIs);

  TDataSetHelper = class helper for TDataSet
  public
    function AsJSONArray: String;
    function AsJSONArrayString: string; deprecated 'Use AsJSONArray';
    function AsJSONObject(AFieldNamePolicy: TFieldNamePolicy = fpLowerCase): String;
    function AsJSONObjectString: string; deprecated 'Use AsJSONObject';
    procedure LoadFromJSONObject(AJSONObject: TJSONObject;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    procedure LoadFromJSONObject(AJSONObject: TJSONObject;
      AIgnoredFields: TArray<string>;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    procedure LoadFromJSONArray(AJSONArray: String;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.
      fpLowerCase); overload;
    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONArrayString(AJSONArrayString: string;
      AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONArray(AJSONArray: TJSONArray;
      AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string); overload;
    procedure LoadFromJSONObjectString(AJSONObjectString: string;
      AIgnoredFields: TArray<string>); overload;
    procedure AppendFromJSONArrayString(AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString(AJSONArrayString: string;
      AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy = TFieldNamePolicy.fpLowerCase); overload;
    function AsObjectList<T: class, constructor>(CloseAfterScroll
      : boolean = false): TObjectList<T>;
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
      ACloseDataSetAfterScroll: boolean = True);
  end;

implementation

uses
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JSONDataObjects,
  MVCFramework.Serializer.Intf;

{ TDataSetHelper }

function TDataSetHelper.AsJSONArray: String;
var
  lSerializer: IMVCSerializer;
begin
  Result := '[]';
  if not Eof then
  begin
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    Result := lSerializer.SerializeDataSet(Self, [], ncLowerCase);
    // TDataSetUtils.DataSetToJSONArray(Self, JArr, false);
  end;
end;

function TDataSetHelper.AsJSONArrayString: string;
begin
  Result := AsJSONArray;
end;

function TDataSetHelper.AsJSONObject(AFieldNamePolicy: TFieldNamePolicy): String;
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

function TDataSetHelper.AsObjectList<T>(CloseAfterScroll: boolean): TObjectList<T>;
var
  Objs: TObjectList<T>;
begin
  Objs := TObjectList<T>.Create(True);
  try
    TDataSetUtils.DataSetToObjectList<T>(Self, Objs, CloseAfterScroll);
    Result := Objs;
  except
    FreeAndNil(Objs);
    raise;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: String;
  AFieldNamePolicy: TFieldNamePolicy);
var
  lSerializer: IMVCSerializer;
begin
  Self.DisableControls;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    lSerializer.DeserializeDataSet(AJSONArray, Self, nil, ncAsIs);
    // Mapper.JSONArrayToDataSet(AJSONArray, Self, TArray<string>.Create(), false,
    // AFieldNamePolicy);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: TJSONArray;
  AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy);
begin
  Self.DisableControls;
  try
    raise Exception.Create('Not Implemented');
    // Mapper.JSONArrayToDataSet(AJSONArray, Self, AIgnoredFields, false, AFieldNamePolicy);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string;
  AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy);
begin
  AppendFromJSONArrayString(AJSONArrayString, AIgnoredFields, AFieldNamePolicy);
end;

procedure TDataSetHelper.LoadFromJSONArrayString(AJSONArrayString: string; AFieldNamePolicy: TFieldNamePolicy);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<String>.Create(), AFieldNamePolicy);
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string;
  AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy);
// var
// JV: TJSONValue;
// lJArr: TJDOJsonArray;
begin
  // raise Exception.Create('Not Implemented');

  // lJArr := TJsonBaseObject.Parse(AJSONArrayString) as TJDOJsonArray;
  LoadFromJSONArray(AJSONArrayString, AFieldNamePolicy);

  // JV := TJSONObject.ParseJSONValue(AJSONArrayString);
  // try
  // if JV is TJSONArray then
  // LoadFromJSONArray(TJSONArray(JV), AIgnoredFields, AFieldNamePolicy)
  // else
  // raise Exception.Create('Expected JSONArray in LoadFromJSONArrayString');
  // finally
  // JV.Free;
  // end;
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create());
end;

procedure TDataSetHelper.LoadFromJSONObject(AJSONObject: TJSONObject;
  AIgnoredFields: TArray<string>; AFieldNamePolicy: TFieldNamePolicy);
begin
  raise Exception.Create('Not Implemented');
  // Mapper.JSONObjectToDataSet(AJSONObject, Self, AIgnoredFields, false,
  // AFieldNamePolicy);
end;

procedure TDataSetHelper.LoadFromJSONObjectString(AJSONObjectString: string;
  AIgnoredFields: TArray<string>);
var
  lSerializer: IMVCSerializer;
begin
  lSerializer := TMVCJsonDataObjectsSerializer.Create;
  lSerializer.DeserializeDataSetRecord(AJSONObjectString, Self, nil, ncAsIs);

  // JV := TJSONObject.ParseJSONValue(AJSONObjectString);
  // try
  // if JV is TJSONObject then
  // LoadFromJSONObject(TJSONObject(JV), AIgnoredFields)
  // else
  // raise EMapperException.Create
  // ('Extected JSONObject in LoadFromJSONObjectString');
  // finally
  // JV.Free;
  // end;
end;

procedure TDataSetHelper.LoadFromJSONObject(AJSONObject: TJSONObject;
  AFieldNamePolicy: TFieldNamePolicy);
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

class procedure TDataSetUtils.DataSetToObject(ADataSet: TDataSet;
  AObject: TObject);
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
        FoundAttribute := True;
        mf := MVCColumnAttribute(_attribute);
        _dict.Add(_field.Name, mf.FieldName);
        _keys.Add(_field.Name, mf.IsPK);
      end
      else if _attribute is MVCDoNotSerializeAttribute then
        FoundTransientAttribute := True;
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
    case _field.PropertyType.TypeKind of
      tkEnumeration: // tristan
        begin
          if _field.PropertyType.Handle = TypeInfo(boolean) then
          begin
            case ADataSet.FieldByName(field_name).DataType of
              ftInteger, ftSmallint, ftLargeint:
                begin
                  Value := (ADataSet.FieldByName(field_name).AsInteger = 1);
                end;
              ftBoolean:
                begin
                  Value := ADataSet.FieldByName(field_name).AsBoolean;
                end;
            else
              Continue;
            end;
          end;
        end;
      tkInteger:
        Value := ADataSet.FieldByName(field_name).AsInteger;
      tkInt64:
        Value := ADataSet.FieldByName(field_name).AsLargeInt;
      tkFloat:
        Value := ADataSet.FieldByName(field_name).AsFloat;
      tkString:
        Value := ADataSet.FieldByName(field_name).AsString;
      tkUString, tkWChar, tkLString, tkWString:
        Value := ADataSet.FieldByName(field_name).AsWideString;
    else
      Continue;
    end;
    _field.SetValue(AObject, Value);
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

end.
