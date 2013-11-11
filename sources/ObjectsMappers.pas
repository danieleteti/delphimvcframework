{ *******************************************************************************
  Copyright 2010-2013 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  ******************************************************************************** }
unit ObjectsMappers;

interface

uses
  System.RTTI,
  System.IOUtils,
  DBXPLatform,
  DBXCommon,
  DB,
  Generics.Collections,
  DBXJSON,
  SqlExpr,
  DuckListU;

type
  Mapper = class
  strict private
    class var ctx: TRTTIContext;

  private
    class function InternalExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject;
      WithResult: boolean): Int64;
    class function GetKeyName(const ARttiField: TRttiField; AType: TRttiType): string; overload;
    class function GetKeyName(const ARttiProp: TRttiProperty; AType: TRttiType): string; overload;
    class procedure InternalJSONObjectToObject(ctx: TRTTIContext; AJSONObject: TJSONObject;
      AObject: TObject); static;

  public
    class function HasAttribute<T: class>(ARTTIMember: TRttiMember): boolean; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiMember; out AAttribute: T)
      : boolean; overload;

    ///
    /// Do not restore nested classes
    ///
    class function JSONObjectToObject<T: constructor, class>(AJSONObject: TJSONObject): T;
      overload; static;
    class function JSONObjectToObject(Clazz: TClass; AJSONObject: TJSONObject): TObject;
      overload; static;
    class function JSONObjectToObject(ClazzName: string; AJSONObject: TJSONObject): TObject;
      overload; static;
    class function JSONObjectToObjectFields<T: constructor, class>(AJSONObject: TJSONObject)
      : T; static;
    class procedure ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant); static;
    class procedure DataSetToObject(ADataSet: TDataSet; AObject: TObject);
    class function ObjectFieldsToJSONObject(AObject: TObject): TJSONObject;
    class function ObjectToJSONObject(AObject: TObject; AIgnoredProperties: array of string)
      : TJSONObject; overload;
    class function ObjectToJSONObjectFields(AObject: TObject; AIgnoredProperties: array of string)
      : TJSONObject; overload;
    class function ObjectToJSONObject(AObject: TObject): TJSONObject; overload;
    class function ObjectToJSONArray(AObject: TObject): TJSONArray;
    class function JSONArrayToObjectList<T: class, constructor>(AJSONArray: TJSONArray;
      AInstanceOwner: boolean = True; AOwnsChildObjects: boolean = True): TObjectList<T>;
    class procedure ReaderToObject(AReader: TDBXReader; AObject: TObject);
    class procedure ReaderToObjectList<T: class, constructor>(AReader: TDBXReader;
      AObjectList: TObjectList<T>);
    class procedure ReaderToJSONObject(AReader: TDBXReader; AJSONObject: TJSONObject;
      AReaderInstanceOwner: boolean = True);
    class procedure DataSetToJSONObject(ADataSet: TDataSet; AJSONObject: TJSONObject;
      ADataSetInstanceOwner: boolean = True);
    class procedure DataSetToObjectList<T: class, constructor>(ADataSet: TDataSet;
      AObjectList: TObjectList<T>; ACloseDataSetAfterScroll: boolean = True);
    class function DataSetToJSONArrayOf<T: class, constructor>(ADataSet: TDataSet): TJSONArray;
    class procedure ReaderToList<T: class, constructor>(AReader: TDBXReader; AList: IWrappedList);
    class procedure ReaderToJSONArray(AReader: TDBXReader; AJSONArray: TJSONArray;
      AReaderInstanceOwner: boolean = True);
    class procedure DataSetToJSONArray(ADataSet: TDataSet; AJSONArray: TJSONArray;
      ADataSetInstanceOwner: boolean = True);
    // class procedure DataSetRowToXML(ADataSet: TDataSet; Row: IXMLNode;
    // ADataSetInstanceOwner: boolean = True);
    // class procedure DataSetToXML(ADataSet: TDataSet; XMLDocument: String;
    // ADataSetInstanceOwner: boolean = True);
    class function ObjectListToJSONArray<T: class>(AList: TObjectList<T>;
      AOwnsInstance: boolean = false): TJSONArray;
    class function ObjectListToJSONArrayOfJSONArray<T: class, constructor>(AList: TObjectList<T>)
      : TJSONArray;
    class function GetProperty(Obj: TObject; const PropertyName: string): TValue; static;
    class function ExecuteSQLQueryNoResult(AQuery: TSQLQuery; AObject: TObject): Int64;
    class procedure ExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject = nil);
    class function ExecuteSQLQueryAsObjectList<T: class, constructor>(AQuery: TSQLQuery;
      AObject: TObject = nil): TObjectList<T>;
    /// ///
    class function CreateQuery(AConnection: TSQLConnection; ASQL: string): TSQLQuery;
    // SAFE TJSONObject getter
    class function GetPair(JSONObject: TJSONObject; PropertyName: string): TJSONPair;
    class function GetStringDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: string = ''): string;
    class function GetNumberDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Extended = 0): Extended;
    class function GetJSONObj(JSONObject: TJSONObject; PropertyName: string): TJSONObject;
    class function GetJSONArray(JSONObject: TJSONObject; PropertyName: string): TJSONArray;
    class function GetIntegerDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Integer = 0): Integer;
    class function GetInt64Def(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Int64 = 0): Int64;
    class function GetBooleanDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: boolean = false): boolean;
  end;

  MapperTransientAttribute = class(TCustomAttribute)

  end;

  DoNotSerializeAttribute = class(TCustomAttribute)

  end;

  MapperItemsClassType = class(TCustomAttribute)
  private
    FValue: TClass;
    procedure SetValue(const Value: TClass);

  published
    constructor Create(Value: TClass);
    property Value: TClass read FValue write SetValue;
  end;

  TJSONNameCase = (JSONNameUpperCase, JSONNameLowerCase);

  HideInGrids = class(TCustomAttribute)

  end;

  StringValueAttribute = class abstract(TCustomAttribute)
  private
    FValue: string;
    procedure SetValue(const Value: string);

  public
    constructor Create(Value: string);

  published
    property Value: string read FValue write SetValue;
  end;

  FormatFloatValue = class(StringValueAttribute)

  end;

  FormatDateTimeValue = class(StringValueAttribute)

  end;

  MapperJSONNaming = class(TCustomAttribute)
  private
    FJSONKeyCase: TJSONNameCase;
    function GetKeyCase: TJSONNameCase;

  public
    constructor Create(JSONKeyCase: TJSONNameCase);
    property KeyCase: TJSONNameCase read GetKeyCase;
  end;

  MapperJSONSer = class(TCustomAttribute)
  private
    FName: string;
    function GetName: string;

  public
    constructor Create(AName: string);
    property name: string read GetName;
  end;

  MapperColumnAttribute = class(TCustomAttribute)
  private
    FFieldName: string;
    FIsPK: boolean;
    procedure SetFieldName(const Value: string);
    procedure SetIsPK(const Value: boolean);

  public
    constructor Create(AFieldName: string; AIsPK: boolean = false);
    property FieldName: string read FFieldName write SetFieldName;
    property IsPK: boolean read FIsPK write SetIsPK;
  end;

  TGridColumnAlign = (caLeft, caCenter, caRight);

  GridColumnProps = class(TCustomAttribute)
  private
    FCaption: string;
    FAlign: TGridColumnAlign;
    FWidth: Integer;
    function GetAlignAsString: string;

  public
    constructor Create(ACaption: string; AAlign: TGridColumnAlign = caCenter;
      AWidth: Integer = -1);
    property Caption: string read FCaption;
    property Align: TGridColumnAlign read FAlign;
    property AlignAsString: string read GetAlignAsString;
    property Width: Integer read FWidth;
  end;

function ISODateTimeToString(ADateTime: TDateTime): string;
function ISODateToString(ADate: TDateTime): string;
function ISOTimeToString(ATime: TTime): string;

function ISOStrToDateTime(DateTimeAsString: string): TDateTime;
function ISOStrToDate(DateAsString: string): TDate;
function ISOStrToTime(TimeAsString: string): TTime;


// function ISODateToStr(const ADate: TDate): String;
//
// function ISOTimeToStr(const ATime: TTime): String;

implementation

uses
  TypInfo,
  SysUtils,
  FmtBcd,
  Math,
  SqlTimSt,
  DateUtils,
  Classes,
  RTTIUtilsU,
  Xml.adomxmldom;
{ Mapper }

function ISOTimeToString(ATime: TTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('hh:nn:ss', ATime, fs);
end;

function ISODateToString(ADate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', ADate);
end;

function ISODateTimeToString(ADateTime: TDateTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOStrToDateTime(DateTimeAsString: string): TDateTime;
begin
  Result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)), StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)), StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function ISOStrToTime(TimeAsString: string): TTime;
begin
  Result := EncodeTime(StrToInt(Copy(TimeAsString, 1, 2)), StrToInt(Copy(TimeAsString, 4, 2)),
    StrToInt(Copy(TimeAsString, 7, 2)), 0);
end;

function ISOStrToDate(DateAsString: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(DateAsString, 1, 4)), StrToInt(Copy(DateAsString, 6, 2)),
    StrToInt(Copy(DateAsString, 9, 2)));
  // , StrToInt
  // (Copy(DateAsString, 12, 2)), StrToInt(Copy(DateAsString, 15, 2)),
  // StrToInt(Copy(DateAsString, 18, 2)), 0);
end;


// function ISODateToStr(const ADate: TDate): String;
// begin
// Result := FormatDateTime('YYYY-MM-DD', ADate);
// end;
//
// function ISOTimeToStr(const ATime: TTime): String;
// begin
// Result := FormatDateTime('HH:nn:ss', ATime);
// end;

class function Mapper.CreateQuery(AConnection: TSQLConnection; ASQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.SQLConnection := AConnection;
  Result.CommandText := ASQL;
end;

class procedure Mapper.DataSetToJSONArray(ADataSet: TDataSet; AJSONArray: TJSONArray;
  ADataSetInstanceOwner: boolean);
var
  Obj: TJSONObject;
begin
  while not ADataSet.Eof do
  begin
    Obj := TJSONObject.Create;
    AJSONArray.AddElement(Obj);
    DataSetToJSONObject(ADataSet, Obj, false);
    ADataSet.Next;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
end;

class function Mapper.DataSetToJSONArrayOf<T>(ADataSet: TDataSet): TJSONArray;
var
  list: TObjectList<T>;
begin
  list := TObjectList<T>.Create;
  try
    Mapper.DataSetToObjectList<T>(ADataSet, list);
    Result := Mapper.ObjectListToJSONArray<T>(list);
  finally
    list.Free;
  end;
end;

class procedure Mapper.DataSetToJSONObject(ADataSet: TDataSet; AJSONObject: TJSONObject;
  ADataSetInstanceOwner: boolean);
var
  I: Integer;
  key: string;
  dt: TDateTime;
  tt: TTime;
  Time: TTimeStamp;
  ts: TSQLTimeStamp;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    key := LowerCase(ADataSet.Fields[I].FieldName);
    case ADataSet.Fields[I].DataType of
      TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint:
        AJSONObject.AddPair(key, TJSONNumber.Create(ADataSet.Fields[I].AsInteger));
      TFieldType.ftLargeint:
        begin
          AJSONObject.AddPair(key, TJSONNumber.Create(ADataSet.Fields[I].AsLargeInt));
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        AJSONObject.AddPair(key, TJSONNumber.Create(ADataSet.Fields[I].AsFloat));
      ftString, ftWideString, ftMemo:
        AJSONObject.AddPair(key, ADataSet.Fields[I].AsWideString);
      TFieldType.ftDate:
        begin
          if not ADataSet.Fields[I].IsNull then
          begin
            AJSONObject.AddPair(key, ISODateToString(ADataSet.Fields[I].AsDateTime));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;
      TFieldType.ftDateTime:
        begin
          if not ADataSet.Fields[I].IsNull then
          begin
            AJSONObject.AddPair(key, ISODateTimeToString(ADataSet.Fields[I].AsDateTime));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;
      TFieldType.ftTimeStamp:
        begin
          if not ADataSet.Fields[I].IsNull then
          begin
            ts := ADataSet.Fields[I].AsSQLTimeStamp;
            AJSONObject.AddPair(key, SQLTimeStampToStr('hh:nn:ss', ts));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;
      TFieldType.ftCurrency:
        begin
          if not ADataSet.Fields[I].IsNull then
          begin
            AJSONObject.AddPair(key, FormatCurr('0.00##', ADataSet.Fields[I].AsCurrency));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;
      TFieldType.ftFMTBcd:
        begin
          if not ADataSet.Fields[I].IsNull then
          begin
            AJSONObject.AddPair(key, TJSONNumber.Create(BcdToDouble(ADataSet.Fields[I].AsBcd)));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;

    else
      raise Exception.Create('Cannot find type for field ' + key);
    end;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);

end;

class procedure Mapper.DataSetToObject(ADataSet: TDataSet; AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  _attribute: TCustomAttribute;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MapperColumnAttribute;
  field_name: string;
  Value: TValue;
  FoundAttribute: boolean;
  FoundTransientAttribute: boolean;
begin
  _dict := TDictionary<string, string>.Create();
  _keys := TDictionary<string, boolean>.Create();
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
  begin
    FoundAttribute := false;
    FoundTransientAttribute := false;
    for _attribute in _field.GetAttributes do
    begin
      if _attribute is MapperColumnAttribute then
      begin
        FoundAttribute := True;
        mf := MapperColumnAttribute(_attribute);
        _dict.Add(_field.Name, mf.FieldName);
        _keys.Add(_field.Name, mf.IsPK);
      end
      else if _attribute is MapperTransientAttribute then
        FoundTransientAttribute := True;
    end;
    if not(FoundAttribute and FoundTransientAttribute) then
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
      tkInt64, tkInteger:
        Value := ADataSet.FieldByName(field_name).AsInteger;
      tkFloat:
        Value := ADataSet.FieldByName(field_name).AsFloat;
      tkString, tkUString, tkWChar, tkLString, tkWString:
        Value := ADataSet.FieldByName(field_name).AsString;
    end;
    _field.SetValue(AObject, Value);
  end;
  _dict.Free;
  _keys.Free;
end;

class function Mapper.ObjectListToJSONArray<T>(AList: TObjectList<T>; AOwnsInstance: boolean)
  : TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
      Result.AddElement(ObjectToJSONObject(AList[I]));
  if AOwnsInstance then
    AList.Free;
end;

class function Mapper.ObjectListToJSONArrayOfJSONArray<T>(AList: TObjectList<T>): TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to AList.Count - 1 do
    Result.AddElement(ObjectToJSONArray(AList[I]));
end;

class procedure Mapper.ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant);
begin
  Value := GetProperty(Obj, Field.FieldName).AsVariant;
end;

class function Mapper.ObjectToJSONArray(AObject: TObject): TJSONArray;
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  f: string;
  JSONArray: TJSONArray;
  o: TObject;
  list: IWrappedList;
  arr: TJSONArray;
  Obj: TObject;
begin
  JSONArray := TJSONArray.Create;
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
  begin
    if HasAttribute<DoNotSerializeAttribute>(_field) then
      Continue;
    f := GetKeyName(_field, _type);
    case _field.PropertyType.TypeKind of
      tkEnumeration:
        begin
          if _field.PropertyType.QualifiedName = 'System.Boolean' then
          begin
            if _field.GetValue(AObject).AsBoolean then
              JSONArray.AddElement(TJSONTrue.Create)
            else
              JSONArray.AddElement(TJSONFalse.Create)
          end;
        end;
      tkInteger, tkInt64:
        JSONArray.AddElement(TJSONNumber.Create(_field.GetValue(AObject).AsInteger));
      tkFloat:
        begin
          if _field.PropertyType.QualifiedName = 'System.TDate' then
            JSONArray.AddElement(TJSONString.Create(ISODateToString(_field.GetValue(AObject)
              .AsExtended)))
          else if _field.PropertyType.QualifiedName = 'System.TDateTime' then
            JSONArray.AddElement(TJSONString.Create(ISODateTimeToString(_field.GetValue(AObject)
              .AsExtended)))
          else if _field.PropertyType.QualifiedName = 'System.TTime' then
            JSONArray.AddElement(TJSONString.Create(ISOTimeToString(_field.GetValue(AObject)
              .AsExtended)))
          else
            JSONArray.AddElement(TJSONNumber.Create(_field.GetValue(AObject).AsExtended));
        end;
      tkString, tkLString, tkWString, tkUString:
        JSONArray.AddElement(TJSONString.Create(_field.GetValue(AObject).AsString));
      tkClass:
        begin
          o := _field.GetValue(AObject).AsObject;
          if Assigned(o) then
          begin
            list := nil;
            if TDuckTypedList.CanBeWrappedAsList(o) then
              list := WrapAsList(o);
            if Assigned(list) then
            begin
              arr := TJSONArray.Create;
              JSONArray.AddElement(arr);
              for Obj in list do
              begin
                arr.AddElement(ObjectToJSONObject(Obj));
              end;
            end
            else
            begin
              JSONArray.AddElement(ObjectToJSONObject(_field.GetValue(AObject).AsObject));
            end;
          end
          else
            JSONArray.AddElement(TJSONNull.Create);
        end;
    end;
  end;
  Result := JSONArray;
end;

class function Mapper.ObjectToJSONObject(AObject: TObject; AIgnoredProperties: array of string)
  : TJSONObject;
var
  _type: TRttiType;
  _properties: TArray<TRttiProperty>;
  _property: TRttiProperty;
  f: string;
  JSONObject: TJSONObject;
  arr: TJSONArray;
  list: IWrappedList;
  Obj, o: TObject;
  DoNotSerializeThis: boolean;
  I: Integer;
  ThereAreIgnoredProperties: boolean;
begin
  ThereAreIgnoredProperties := Length(AIgnoredProperties) > 0;
  JSONObject := TJSONObject.Create;
  _type := ctx.GetType(AObject.ClassInfo);
  _properties := _type.GetProperties;
  for _property in _properties do
  begin
    // f := LowerCase(_property.Name);
    f := GetKeyName(_property, _type);
    // Delete(f, 1, 1);
    if ThereAreIgnoredProperties then
    begin
      DoNotSerializeThis := false;
      for I := low(AIgnoredProperties) to high(AIgnoredProperties) do
        if SameText(f, AIgnoredProperties[I]) then
        begin
          DoNotSerializeThis := True;
          Break;
        end;
      if DoNotSerializeThis then
        Continue;
    end;

    if HasAttribute<DoNotSerializeAttribute>(_property) then
      Continue;

    case _property.PropertyType.TypeKind of
      tkInteger, tkInt64:
        JSONObject.AddPair(f, TJSONNumber.Create(_property.GetValue(AObject).AsInteger));
      tkFloat:
        begin
          if _property.PropertyType.QualifiedName = 'System.TDate' then
          begin
            if _property.GetValue(AObject).AsExtended = 0 then
              JSONObject.AddPair(f, TJSONNull.Create)
            else
              JSONObject.AddPair(f, ISODateToString(_property.GetValue(AObject).AsExtended))
          end
          else if _property.PropertyType.QualifiedName = 'System.TDateTime' then
          begin
            if _property.GetValue(AObject).AsExtended = 0 then
              JSONObject.AddPair(f, TJSONNull.Create)
            else
              JSONObject.AddPair(f, ISODateTimeToString(_property.GetValue(AObject).AsExtended))
          end
          else if _property.PropertyType.QualifiedName = 'System.TTime' then
            JSONObject.AddPair(f, ISOTimeToString(_property.GetValue(AObject).AsExtended))
          else
            JSONObject.AddPair(f, TJSONNumber.Create(_property.GetValue(AObject).AsExtended));
        end;
      tkString, tkLString, tkWString, tkUString:
        JSONObject.AddPair(f, _property.GetValue(AObject).AsString);
      tkEnumeration:
        begin
          if _property.PropertyType.QualifiedName = 'System.Boolean' then
          begin
            if _property.GetValue(AObject).AsBoolean then
              JSONObject.AddPair(f, TJSONTrue.Create)
            else
              JSONObject.AddPair(f, TJSONFalse.Create);
          end
          else
          begin
            JSONObject.AddPair(f, TJSONNumber.Create(_property.GetValue(AObject).AsOrdinal));
          end;
        end;
      tkClass:
        begin
          o := _property.GetValue(AObject).AsObject;
          if Assigned(o) then
          begin
            if TDuckTypedList.CanBeWrappedAsList(o) then
            begin
              list := WrapAsList(o);
              if Assigned(list) then
              begin
                arr := TJSONArray.Create;
                JSONObject.AddPair(f, arr);
                for Obj in list do
                begin
                  if Assigned(Obj) then // nil element into the list are not serialized
                    arr.AddElement(ObjectToJSONObject(Obj));
                end;
              end
            end
            else
            begin
              JSONObject.AddPair(f, ObjectToJSONObject(_property.GetValue(AObject).AsObject));
            end;
          end
          else
            JSONObject.AddPair(f, TJSONNull.Create);
        end;
    end;
  end;
  Result := JSONObject;
end;

class function Mapper.ObjectFieldsToJSONObject(AObject: TObject): TJSONObject;
begin
end;

class function Mapper.ObjectToJSONObject(AObject: TObject): TJSONObject;
begin
  Result := ObjectToJSONObject(AObject, []);
end;

class function Mapper.ObjectToJSONObjectFields(AObject: TObject;
  AIgnoredProperties: array of string): TJSONObject;
var
  _type: TRttiType;
  _fields: TArray<TRttiField>;
  _field: TRttiField;
  f: string;
  JSONObject: TJSONObject;
  arr: TJSONArray;
  list: IWrappedList;
  Obj, o: TObject;
  DoNotSerializeThis: boolean;
  I: Integer;
  ThereAreIgnoredProperties: boolean;
begin
  ThereAreIgnoredProperties := Length(AIgnoredProperties) > 0;
  JSONObject := TJSONObject.Create;
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetFields;
  for _field in _fields do
  begin
    // f := LowerCase(_field.Name);
    f := GetKeyName(_field, _type);
    // Delete(f, 1, 1);
    if ThereAreIgnoredProperties then
    begin
      DoNotSerializeThis := false;
      for I := low(AIgnoredProperties) to high(AIgnoredProperties) do
        if SameText(f, AIgnoredProperties[I]) then
        begin
          DoNotSerializeThis := True;
          Break;
        end;
      if DoNotSerializeThis then
        Continue;
    end;
    case _field.FieldType.TypeKind of
      tkInteger, tkInt64:
        JSONObject.AddPair(f, TJSONNumber.Create(_field.GetValue(AObject).AsInteger));
      tkFloat:
        begin
          if _field.FieldType.QualifiedName = 'System.TDate' then
            JSONObject.AddPair(f, ISODateToString(_field.GetValue(AObject).AsExtended))
          else if _field.FieldType.QualifiedName = 'System.TDateTime' then
            JSONObject.AddPair(f, ISODateTimeToString(_field.GetValue(AObject).AsExtended))
          else if _field.FieldType.QualifiedName = 'System.TTime' then
            JSONObject.AddPair(f, ISOTimeToString(_field.GetValue(AObject).AsExtended))
          else
            JSONObject.AddPair(f, TJSONNumber.Create(_field.GetValue(AObject).AsExtended));
        end;
      tkString, tkLString, tkWString, tkUString:
        JSONObject.AddPair(f, _field.GetValue(AObject).AsString);
      tkEnumeration:
        begin
          if _field.FieldType.QualifiedName = 'System.Boolean' then
          begin
            if _field.GetValue(AObject).AsBoolean then
              JSONObject.AddPair(f, TJSONTrue.Create)
            else
              JSONObject.AddPair(f, TJSONFalse.Create);
          end
          else
          begin
            JSONObject.AddPair(f, TJSONNumber.Create(_field.GetValue(AObject).AsOrdinal));
          end;
        end;
      tkClass:
        begin
          o := _field.GetValue(AObject).AsObject;
          if Assigned(o) then
          begin
            list := WrapAsList(o);
            if Assigned(list) then
            begin
              arr := TJSONArray.Create;
              JSONObject.AddPair(f, arr);
              for Obj in list do
              begin
                arr.AddElement(ObjectToJSONObject(Obj));
              end;
            end
            else
            begin
              JSONObject.AddPair(f, ObjectToJSONObject(_field.GetValue(AObject).AsObject));
            end;
          end
          else
            JSONObject.AddPair(f, TJSONNull.Create);
        end;
    end;
  end;
  Result := JSONObject;
end;

class procedure Mapper.ReaderToJSONArray(AReader: TDBXReader; AJSONArray: TJSONArray;
  AReaderInstanceOwner: boolean);
var
  Obj: TJSONObject;
begin
  while AReader.Next do
  begin
    Obj := TJSONObject.Create;
    AJSONArray.AddElement(Obj);
    ReaderToJSONObject(AReader, Obj, false);
  end;
  if AReaderInstanceOwner then
    FreeAndNil(AReader);
end;

class procedure Mapper.ReaderToJSONObject(AReader: TDBXReader; AJSONObject: TJSONObject;
  AReaderInstanceOwner: boolean);
var
  I: Integer;
  key: string;
  dt: TDateTime;
  tt: TTime;
  Time: TTimeStamp;
  ts: TSQLTimeStamp;
begin
  for I := 0 to AReader.ColumnCount - 1 do
  begin
    key := LowerCase(AReader.Value[I].ValueType.Name);
    case AReader.Value[I].ValueType.DataType of
      TDBXDataTypes.Int16Type:
        AJSONObject.AddPair(key, TJSONNumber.Create(AReader.Value[I].AsInt16));
      TDBXDataTypes.Int32Type:
        AJSONObject.AddPair(key, TJSONNumber.Create(AReader.Value[I].AsInt32));
      TDBXDataTypes.Int64Type:
        AJSONObject.AddPair(key, TJSONNumber.Create(AReader.Value[I].AsInt64));
      TDBXDataTypes.DoubleType:
        AJSONObject.AddPair(key, TJSONNumber.Create(AReader.Value[I].AsDouble));
      TDBXDataTypes.AnsiStringType, TDBXDataTypes.WideStringType:
        AJSONObject.AddPair(key, AReader.Value[I].AsString);
      TDBXDataTypes.BcdType:
        AJSONObject.AddPair(key, TJSONNumber.Create(BcdToDouble(AReader.Value[I].AsBcd)));
      TDBXDataTypes.DateType:
        begin
          if not AReader.Value[I].IsNull then
          begin
            Time.Time := 0;
            Time.date := AReader.Value[I].AsDate;
            dt := TimeStampToDateTime(Time);
            AJSONObject.AddPair(key, ISODateToString(dt));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end;
      TDBXDataTypes.TimeType:
        begin
          if not AReader.Value[I].IsNull then
          begin
            ts := AReader.Value[I].AsTimeStamp;
            AJSONObject.AddPair(key, SQLTimeStampToStr('hh:nn:ss', ts));
          end
          else
            AJSONObject.AddPair(key, TJSONNull.Create);
        end
    else
      raise Exception.Create('Cannot find type');
    end;
  end;
  if AReaderInstanceOwner then
    FreeAndNil(AReader);
end;

class procedure Mapper.ReaderToList<T>(AReader: TDBXReader; AList: IWrappedList);
var
  Obj: T;
begin
  while AReader.Next do
  begin
    Obj := T.Create;
    ReaderToObject(AReader, Obj);
    AList.Add(Obj);
  end;
  AReader.Close;
end;

class procedure Mapper.ReaderToObject(AReader: TDBXReader; AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  _attribute: MapperColumnAttribute;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MapperColumnAttribute;
  field_name: string;
  Value: TValue;
  Time: TTimeStamp;
  dt: TDateTime;
  T: TTime;
  ts: TTimeStamp;
  sqlts: TSQLTimeStamp;
begin
  _dict := TDictionary<string, string>.Create();
  _keys := TDictionary<string, boolean>.Create();
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
    if HasAttribute<MapperColumnAttribute>(_field, _attribute) then
    begin
      mf := _attribute;
      _dict.Add(_field.Name, mf.FieldName);
      _keys.Add(_field.Name, mf.IsPK);
    end
    else
    begin
      _dict.Add(_field.Name, _field.Name);
      _keys.Add(_field.Name, false);
    end;

  for _field in _fields do
  begin
    if (not _dict.TryGetValue(_field.Name, field_name)) or (not _field.IsWritable) or
      (HasAttribute<MapperTransientAttribute>(_field)) then
      Continue;
    case _field.PropertyType.TypeKind of
      tkInteger:
        Value := AReader.Value[field_name].AsInt32;
      tkFloat:
        begin
          if AReader.Value[field_name].IsNull then
            Value := 0
          else
          begin
            if AReader.Value[field_name].ValueType.DataType = TDBXDataTypes.DateType then
            begin
              ts.Time := 0;
              ts.date := AReader.Value[field_name].AsDate;
              Value := TimeStampToDateTime(ts);
            end
            else if AReader.Value[field_name].ValueType.DataType = TDBXDataTypes.DoubleType then
              Value := AReader.Value[field_name].AsDouble
            else if AReader.Value[field_name].ValueType.DataType = TDBXDataTypes.BcdType then
              Value := BcdToDouble(AReader.Value[field_name].AsBcd)
            else if AReader.Value[field_name].ValueType.DataType = TDBXDataTypes.TimeType then
            begin
              sqlts := AReader.Value[field_name].AsTimeStamp;
              Value := SQLTimeStampToDateTime(sqlts);
            end
            else
              raise Exception.Create('Unknown tkFloat Type');
          end;
        end;
      tkString, tkUString, tkWChar, tkLString, tkWString:
        begin
          if AReader.Value[field_name].IsNull then
            Value := ''
          else
            Value := AReader.Value[field_name].AsString;
        end;
    else
      raise Exception.Create('Unknown field type for ' + field_name);
    end;
    _field.SetValue(AObject, Value);
  end;
  _dict.Free;
  _keys.Free;
end;

class procedure Mapper.ReaderToObjectList<T>(AReader: TDBXReader; AObjectList: TObjectList<T>);
var
  Obj: T;
begin
  while AReader.Next do
  begin
    Obj := T.Create;
    ReaderToObject(AReader, Obj);
    AObjectList.Add(Obj);
  end;
  AReader.Close;
end;

class function Mapper.GetKeyName(const ARttiField: TRttiField; AType: TRttiType): string;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  // JSONSer property attribute handling
  attrs := ARttiField.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONSer then
      Exit(MapperJSONSer(attr).Name);
  end;

  // JSONNaming class attribute handling
  attrs := AType.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONNaming then
    begin
      case MapperJSONNaming(attr).GetKeyCase of
        JSONNameUpperCase:
          begin
            Exit(UpperCase(ARttiField.Name));
          end;
        JSONNameLowerCase:
          begin
            Exit(LowerCase(ARttiField.Name));
          end;
      end;
    end;
  end;

  // Default
  Result := ARttiField.Name;
end;

class function Mapper.GetBooleanDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: boolean): boolean;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONFalse then
    Exit(false)
  else if pair.JsonValue is TJSONTrue then
    Exit(True)
  else
    raise Exception.Create('Property is not a Boolean Property');
end;

class function Mapper.GetInt64Def(JSONObject: TJSONObject; PropertyName: string;
  DefaultValue: Int64): Int64;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsInt64)
  else
    raise Exception.Create('Property is not an Int64 Property');
end;

class function Mapper.GetIntegerDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: Integer): Integer;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsInt)
  else
    raise Exception.Create('Property is not an Integer Property');
end;

class function Mapper.GetJSONArray(JSONObject: TJSONObject;
  PropertyName: string): TJSONArray;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(nil);
  if pair.JsonValue is TJSONArray then
    Exit(TJSONArray(pair.JsonValue))
  else
    raise Exception.Create('Property is not a JSONArray');

end;

class function Mapper.GetJSONObj(JSONObject: TJSONObject;
  PropertyName: string): TJSONObject;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(nil);
  if pair.JsonValue is TJSONObject then
    Exit(TJSONObject(pair.JsonValue))
  else
    raise Exception.Create('Property is not a JSONObject');
end;

class function Mapper.GetKeyName(const ARttiProp: TRttiProperty; AType: TRttiType): string;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  // JSONSer property attribute handling
  attrs := ARttiProp.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONSer then
      Exit(MapperJSONSer(attr).Name);
  end;

  // JSONNaming class attribute handling
  attrs := AType.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONNaming then
    begin
      case MapperJSONNaming(attr).GetKeyCase of
        JSONNameUpperCase:
          begin
            Exit(UpperCase(ARttiProp.Name));
          end;
        JSONNameLowerCase:
          begin
            Exit(LowerCase(ARttiProp.Name));
          end;
      end;
    end;
  end;

  // Default
  Result := ARttiProp.Name;
end;

class function Mapper.GetNumberDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: Extended): Extended;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsDouble)
  else
    raise Exception.Create('Property is not a Number Property');
end;

class function Mapper.GetPair(JSONObject: TJSONObject;
  PropertyName: string): TJSONPair;
var
  pair: TJSONPair;
begin
  if not Assigned(JSONObject) then
    raise Exception.Create('JSONObject is nil');
  pair := JSONObject.Get(PropertyName);
  Result := pair;
end;

class function Mapper.GetProperty(Obj: TObject; const PropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARTTIType: TRttiType;
begin
  ARTTIType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARTTIType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARTTIType.ToString]);
  Prop := ARTTIType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(Obj)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
end;

class function Mapper.GetStringDef(JSONObject: TJSONObject; PropertyName,
  DefaultValue: string): string;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONString then
    Exit(TJSONString(pair.JsonValue).Value)
  else
    raise Exception.Create('Property is not a String Property');
end;

class function Mapper.HasAttribute<T>(ARTTIMember: TRttiMember; out AAttribute: T): boolean;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := false;
  attrs := ARTTIMember.GetAttributes;
  for attr in attrs do
    if attr is T then
    begin
      AAttribute := T(attr);
      Exit(True);
    end;
end;

class function Mapper.HasAttribute<T>(ARTTIMember: TRttiMember): boolean;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  Result := false;
  attrs := ARTTIMember.GetAttributes;
  for attr in attrs do
    if attr is T then
      Exit(True);
end;

class function Mapper.JSONArrayToObjectList<T>(AJSONArray: TJSONArray; AInstanceOwner: boolean;
  AOwnsChildObjects: boolean): TObjectList<T>;
var
  I: Integer;
begin
  Result := TObjectList<T>.Create(AOwnsChildObjects);
  if Assigned(AJSONArray) then
  begin
    for I := 0 to AJSONArray.Size - 1 do
      Result.Add(Mapper.JSONObjectToObject<T>(AJSONArray.Get(I) as TJSONObject));
    if AInstanceOwner then
      AJSONArray.Free;
  end;
end;

class procedure Mapper.InternalJSONObjectToObject(ctx: TRTTIContext; AJSONObject: TJSONObject;
  AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  f: string;
  jvalue: TJSONValue;
  v: TValue;
  o: TObject;
  list: IWrappedList;
  oo: TObject;
  I: Integer;
  cref: TClass;
  attr: MapperItemsClassType;
  arr: TJSONArray;
begin
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
  begin
    if ((not _field.IsWritable) and (_field.PropertyType.TypeKind <> tkClass)) or
      (HasAttribute<MapperTransientAttribute>(_field)) then
      Continue;
    f := GetKeyName(_field, _type);
    if Assigned(AJSONObject.Get(f)) then
      jvalue := AJSONObject.Get(f).JsonValue
    else
      Continue;
    case _field.PropertyType.TypeKind of
      tkEnumeration:
        begin
          if _field.PropertyType.QualifiedName = 'System.Boolean' then
          begin
            if jvalue is TJSONTrue then
              _field.SetValue(TObject(AObject), True)
            else if jvalue is TJSONFalse then
              _field.SetValue(TObject(AObject), false)
            else
              raise Exception.Create('Invalid value for property ' + _field.Name);
          end
          else // it is an enumerated value but it's not a boolean.
          begin
            TValue.Make((jvalue as TJSONNumber).AsInt, _field.PropertyType.Handle, v);
            _field.SetValue(TObject(AObject), v);
          end;
        end;
      tkInteger, tkInt64:
        _field.SetValue(TObject(AObject), StrToIntDef(jvalue.Value, 0));
      tkFloat:
        begin
          if _field.PropertyType.QualifiedName = 'System.TDate' then
          begin
            if jvalue is TJSONNull then
              _field.SetValue(TObject(AObject), 0)
            else
              _field.SetValue(TObject(AObject), ISOStrToDateTime(jvalue.Value + ' 00:00:00'))
          end
          else if _field.PropertyType.QualifiedName = 'System.TDateTime' then
          begin
            if jvalue is TJSONNull then
              _field.SetValue(TObject(AObject), 0)
            else
              _field.SetValue(TObject(AObject), ISOStrToDateTime(jvalue.Value))
          end
          else if _field.PropertyType.QualifiedName = 'System.TTime' then
            _field.SetValue(TObject(AObject), ISOStrToTime(jvalue.Value))
          else
            _field.SetValue(TObject(AObject), (jvalue as TJSONNumber).AsDouble)
        end;
      tkString, tkLString, tkWString, tkUString:
        begin
          _field.SetValue(TObject(AObject), jvalue.Value);
        end;
      tkClass: // try to restore child properties... but only if the collection is not nil!!!
        begin
          o := _field.GetValue(TObject(AObject)).AsObject;
          if Assigned(o) then
          begin
            if TDuckTypedList.CanBeWrappedAsList(o) then
            begin // restore collection
              if jvalue is TJSONArray then
              begin
                arr := TJSONArray(jvalue);
                if Mapper.HasAttribute<MapperItemsClassType>(_field, attr) then
                begin
                  cref := attr.Value;
                  list := WrapAsList(o);
                  for I := 0 to arr.Size - 1 do
                  begin
                    list.Add(Mapper.JSONObjectToObject(cref, arr.Get(I) as TJSONObject));
                  end;
                end;
              end
              else
                raise Exception.Create('Cannot restore ' + f +
                  ' because the related json property is not an array');
            end
            else
              raise Exception.Create('Property cannot be wrapped as list');
          end;
        end;
    end;
  end;
end;

class function Mapper.JSONObjectToObject(Clazz: TClass; AJSONObject: TJSONObject): TObject;
var
  AObject: TObject;
begin
  AObject := TRTTIUtils.CreateObject(Clazz.QualifiedClassName);
  try
    InternalJSONObjectToObject(ctx, AJSONObject, AObject);
    Result := AObject;
  except
    AObject.Free;
    AObject := nil;
    Result := nil;
  end;
end;

class function Mapper.JSONObjectToObject(ClazzName: string; AJSONObject: TJSONObject): TObject;
var
  AObject: TObject;
  _rttiType: TRttiType;
begin
  _rttiType := Mapper.ctx.FindType(ClazzName);
  if Assigned(_rttiType) then
  begin
    AObject := TRTTIUtils.CreateObject(_rttiType);
    try
      InternalJSONObjectToObject(ctx, AJSONObject, AObject);
      Result := AObject;
    except
      AObject.Free;
      AObject := nil;
      Result := nil;
    end;
  end
  else
    raise Exception.CreateFmt('Class not found [%s]', [ClazzName]);
end;

class function Mapper.JSONObjectToObject<T>(AJSONObject: TJSONObject): T;
begin
  Result := Mapper.JSONObjectToObject(T.QualifiedClassName, AJSONObject) as T;
  // Result := JSONObjectToObject(TObject.ClassInfo, AJSONObject);
end;

class function Mapper.JSONObjectToObjectFields<T>(AJSONObject: TJSONObject): T;
var
  _type: TRttiType;
  _fields: TArray<TRttiField>;
  _field: TRttiField;
  f: string;
  AObject: T;
  jvalue: TJSONValue;
begin
  AObject := T.Create;
  try
    _type := ctx.GetType(AObject.ClassInfo);
    _fields := _type.GetFields;
    for _field in _fields do
    begin
      f := LowerCase(_field.Name);
      Delete(f, 1, 1);
      if Assigned(AJSONObject.Get(f)) then
        jvalue := AJSONObject.Get(f).JsonValue
      else
        Continue;
      case _field.FieldType.TypeKind of
        tkInteger, tkInt64:
          _field.SetValue(TObject(AObject), StrToIntDef(jvalue.Value, 0));
        tkFloat:
          begin
            if _field.FieldType.QualifiedName = 'System.TDate' then
              _field.SetValue(TObject(AObject), StrToDate(jvalue.Value))
            else if _field.FieldType.QualifiedName = 'System.TDateTime' then
              _field.SetValue(TObject(AObject), StrToDateTime(jvalue.Value))
            else
              _field.SetValue(TObject(AObject), (jvalue as TJSONNumber).AsDouble)
          end;
        tkString, tkLString, tkWString, tkUString:
          begin
            _field.SetValue(TObject(AObject), jvalue.Value);
          end;
      end;
    end;
    Result := AObject;
  except
    AObject.Free;
    AObject := nil;
    Result := nil;
  end;
end;

class procedure Mapper.DataSetToObjectList<T>(ADataSet: TDataSet; AObjectList: TObjectList<T>;
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
//
// class procedure Mapper.DataSetToXML(ADataSet: TDataSet;
// XMLDocument: String; ADataSetInstanceOwner: boolean);
// var
// Xml: IXMLDocument;
// Row: IXMLNode;
// begin
// DefaultDOMVendor := 'ADOM XML v4';
// Xml := NewXMLDocument();
// while not ADataSet.Eof do
// begin
// Row := Xml.CreateNode('row');
// // Row := Xml.DocumentElement.AddChild('row');
// // DataSetRowToXML(ADataSet, Row, false);
// Xml.ChildNodes.Add(Row);
// break;
// ADataSet.Next;
// end;
// if ADataSetInstanceOwner then
// FreeAndNil(ADataSet);
// Xml.SaveToXML(XMLDocument);
// end;
//
// class procedure Mapper.DataSetRowToXML(ADataSet: TDataSet;
// Row: IXMLNode; ADataSetInstanceOwner: boolean);
// var
// I: Integer;
// key: string;
// dt: TDateTime;
// tt: TTime;
// Time: TTimeStamp;
// ts: TSQLTimeStamp;
// begin
// for I := 0 to ADataSet.FieldCount - 1 do
// begin
// key := LowerCase(ADataSet.Fields[I].FieldName);
// case ADataSet.Fields[I].DataType of
// TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint:
// Row.Attributes[key] := ADataSet.Fields[I].AsInteger;
// // AJSONObject.AddPair(key, TJSONNumber.Create(ADataSet.Fields[I].AsInteger));
// TFieldType.ftLargeint:
// begin
// Row.Attributes[key] := ADataSet.Fields[I].AsLargeInt;
// end;
// TFieldType.ftSingle, TFieldType.ftFloat:
// Row.Attributes[key] := ADataSet.Fields[I].AsFloat;
// ftString, ftWideString, ftMemo:
// Row.Attributes[key] := ADataSet.Fields[I].AsWideString;
// TFieldType.ftDate:
// begin
// if not ADataSet.Fields[I].IsNull then
// begin
// Row.Attributes[key] := ISODateToString(ADataSet.Fields[I].AsDateTime);
// end
// end;
// TFieldType.ftDateTime:
// begin
// if not ADataSet.Fields[I].IsNull then
// begin
// Row.Attributes[key] := ISODateTimeToString(ADataSet.Fields[I].AsDateTime);
// end
// end;
// TFieldType.ftTimeStamp:
// begin
// if not ADataSet.Fields[I].IsNull then
// begin
// ts := ADataSet.Fields[I].AsSQLTimeStamp;
// Row.Attributes[key] := SQLTimeStampToStr('hh:nn:ss', ts);
// end
// end;
// TFieldType.ftCurrency:
// begin
// if not ADataSet.Fields[I].IsNull then
// begin
// Row.Attributes[key] := FormatCurr('0.00##', ADataSet.Fields[I].AsCurrency);
// end
// end;
// TFieldType.ftFMTBcd:
// begin
// if not ADataSet.Fields[I].IsNull then
// begin
// Row.Attributes[key] := BcdToDouble(ADataSet.Fields[I].AsBcd);
// end
// end
// else
// raise Exception.Create('Cannot find type for field ' + key);
// end;
// end;
// if ADataSetInstanceOwner then
// FreeAndNil(ADataSet);
// end;

class function Mapper.InternalExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject;
  WithResult: boolean): Int64;
var
  I: Integer;
  pname: string;
  _rttiType: TRttiType;
  obj_fields: TArray<TRttiProperty>;
  obj_field: TRttiProperty;
  obj_field_attrs: TArray<TCustomAttribute>;
  obj_field_attr: MapperColumnAttribute;
  Map: TObjectDictionary<string, TRttiProperty>;
  f: TRttiProperty;
  fv: TValue;
begin
  Map := TObjectDictionary<string, TRttiProperty>.Create;
  try
    if Assigned(AObject) then
    begin
      _rttiType := ctx.GetType(AObject.ClassType);
      obj_fields := _rttiType.GetProperties;
      for obj_field in obj_fields do
      begin
        if HasAttribute<MapperColumnAttribute>(obj_field, obj_field_attr) then
        begin
          Map.Add(MapperColumnAttribute(obj_field_attr).FieldName, obj_field);
        end
        else
        begin
          Map.Add(LowerCase(obj_field.Name), obj_field);
        end
      end;
    end;
    for I := 0 to AQuery.Params.Count - 1 do
    begin
      pname := AQuery.Params[I].Name;
      if Map.TryGetValue(pname, f) then
      begin
        fv := f.GetValue(AObject);
        AQuery.Params[I].Value := fv.AsVariant;
      end
      else
      begin
        AQuery.Params[I].Clear;
        AQuery.Params[I].DataType := ftString; // just to make dbx happy

      end;
    end;
    Result := 0;
    if WithResult then
      AQuery.Open
    else
      Result := AQuery.ExecSQL;
  finally
    Map.Free;
  end;
end;

class function Mapper.ExecuteSQLQueryNoResult(AQuery: TSQLQuery; AObject: TObject): Int64;
begin
  Result := InternalExecuteSQLQuery(AQuery, AObject, false);
end;

class procedure Mapper.ExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject);
begin
  InternalExecuteSQLQuery(AQuery, AObject, True);
end;

class function Mapper.ExecuteSQLQueryAsObjectList<T>(AQuery: TSQLQuery; AObject: TObject)
  : TObjectList<T>;
begin
  ExecuteSQLQuery(AQuery, AObject);
  Result := TObjectList<T>.Create(True);
  DataSetToObjectList<T>(AQuery, Result);
end;
{ MappedField }

constructor MapperColumnAttribute.Create(AFieldName: string; AIsPK: boolean);
begin
  inherited Create;
  FFieldName := AFieldName;
  FIsPK := AIsPK;
end;

procedure MapperColumnAttribute.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure MapperColumnAttribute.SetIsPK(const Value: boolean);
begin
  FIsPK := Value;
end;
{ GridColumnProps }

constructor GridColumnProps.Create(ACaption: string; AAlign: TGridColumnAlign; AWidth: Integer);
begin
  inherited Create;
  FCaption := ACaption;
  FAlign := AAlign;

{$IF CompilerVersion >= 23.0}
  FWidth := System.Math.Max(AWidth, 50);

{$ELSE}
  FWidth := Math.Max(AWidth, 50);

{$IFEND}

end;

function GridColumnProps.GetAlignAsString: string;
begin
  case FAlign of
    caLeft:
      Result := 'left';
    caCenter:
      Result := 'center';
    caRight:
      Result := 'right';
  end;
end;

{ JSONSer }

constructor MapperJSONSer.Create(AName: string);
begin
  inherited Create;
  FName := AName;
end;

function MapperJSONSer.GetName: string;
begin
  Result := FName;
end;

{ JSONNaming }

constructor MapperJSONNaming.Create(JSONKeyCase: TJSONNameCase);
begin
  inherited Create;
  FJSONKeyCase := JSONKeyCase;
end;

function MapperJSONNaming.GetKeyCase: TJSONNameCase;
begin
  Result := FJSONKeyCase;
end;

{ StringValueAttribute }

constructor StringValueAttribute.Create(Value: string);
begin
  inherited Create;
  FValue := Value;
end;

procedure StringValueAttribute.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ ItemsClassType }

constructor MapperItemsClassType.Create(Value: TClass);
begin
  inherited Create;
  FValue := Value;
end;

procedure MapperItemsClassType.SetValue(const Value: TClass);
begin
  FValue := Value;
end;

end.
