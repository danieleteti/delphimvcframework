// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit ObjectsMappers;

interface

{$I dmvcframework.inc}


uses
  MVCFramework.Serializer.Intf,
  System.RTTI,
  System.IOUtils,
  DBXPLatform,
  DB,
  Generics.Collections,
{$IFDEF USEDBX}
  Data.SqlExpr,
  DBXCommon,
{$IFEND}
{$IFDEF USEFIREDAC}
  FireDAC.Comp.Client, FireDAC.Stan.Param,
{$ENDIF}
  MVCFramework.DuckTyping, System.SysUtils, System.Classes
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$IFEND}
    , MVCFramework.Patches, MVCFramework.Serializer.Commons
    ;

type
  { ***** Daniele Spinetti ***** }
  TFieldNamePolicy = (fpLowerCase, fpUpperCase, fpAsIs);
  { ***** END - Daniele Spinetti ***** }

  EMapperException = class(Exception)

  end;

  TJSONObjectActionProc = reference to procedure(const AJSONObject
    : TJSONObject);

  Mapper = class
  strict private
    class var ctx: TRTTIContext;

  private
{$IFDEF USEFIREDAC}
    class function InternalExecuteFDQuery(AQuery: TFDQuery; AObject: TObject;
      WithResult: boolean): Int64;
{$ENDIF}
{$IFDEF USEDBX}
    class function InternalExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject;
      WithResult: boolean): Int64;
{$ENDIF}
    class function GetKeyName(const ARttiField: TRttiField; AType: TRttiType)
      : string; overload;
    class function GetKeyName(const ARttiProp: TRttiProperty; AType: TRttiType)
      : string; overload;
    class procedure InternalJSONObjectToObject(ctx: TRTTIContext;
      AJSONObject: TJSONObject; AObject: TObject); static;
    class procedure InternalJSONObjectFieldsToObject(ctx: TRTTIContext;
      AJSONObject: TJSONObject; AObject: TObject); static;

    { following methods are used by the serializer/unserializer to handle with the ser/unser logic }
    class function SerializeFloatProperty(AObject: TObject;
      ARTTIProperty: TRttiProperty): TJSONValue;
    class function SerializeFloatField(AObject: TObject; ARttiField: TRttiField)
      : TJSONValue;
    class function SerializeEnumerationProperty(AObject: TObject;
      ARTTIProperty: TRttiProperty): TJSONValue;
    class function SerializeEnumerationField(AObject: TObject;
      ARttiField: TRttiField): TJSONValue;
    class procedure DeSerializeStringStream(aStream: TStream;
      const aSerializedString: string; aEncoding: string); static;
    class procedure DeSerializeBase64StringStream(aStream: TStream;
      const aBase64SerializedString: string); static;
  public
    class function HasAttribute<T: class>(ARTTIMember: TRttiNamedObject)
      : boolean; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiNamedObject;
      out AAttribute: T): boolean; overload;

    ///
    /// Do not restore nested classes
    ///
    class function JSONObjectToObject<T: constructor, class>
      (AJSONObject: TJSONObject): T; overload; static;
    class function JSONObjectStringToObject<T: constructor, class>
      (const AJSONObjectString: string): T;

    class function JSONObjectToObject(Clazz: TClass; AJSONObject: TJSONObject)
      : TObject; overload; static;
    class procedure LoadJSONObjectToObject<T: class>(AJSONObject: TJSONObject;
      const AObject: T); static;
    class function JSONObjectToObject(ClazzName: string;
      AJSONObject: TJSONObject): TObject; overload; static;
    class function JSONObjectToObjectFields<T: constructor, class>
      (AJSONObject: TJSONObject): T; static;
    class procedure ObjectToDataSet(Obj: TObject; Field: TField;
      var Value: Variant); static;
    class procedure DataSetToObject(ADataSet: TDataSet; AObject: TObject);
    class function ObjectToJSONObject(AObject: TObject;
      AIgnoredProperties: array of string): TJSONObject; overload;
    /// <summary>
    /// Serializes an object to a jsonobject using fields value, not property values. WARNING! This
    /// method generates the $dmvc_classname property in the jsonobject.
    /// </summary>
    class function ObjectToJSONObjectFields(AObject: TObject;
      AIgnoredProperties: array of string): TJSONObject; overload;
    class function ObjectToJSONObjectFieldsString(AObject: TObject;
      AIgnoredProperties: array of string): string; overload;

    /// <summary>
    /// Restore the object stored in the JSON object using the $dmvc_classname property
    /// to know the qualified full class name. Values readed from the json are restored directly to the object fields.
    /// Fields MUST be exists into the json. This kind of deserialization is way more strit than the properties based.
    /// It should not be used to serialize object for a thin client, but to serialize objects that must be deserialized using
    /// the same delphi class. So this method is useful when you are developing a delphi-delphi solution. Exceptions apply.
    /// </summary>
    class function JSONObjectFieldsToObject(AJSONObject: TJSONObject): TObject;
    class procedure LoadJSONObjectFieldsStringToObject(AJSONObjectString: string; AObject: TObject);
    /// <summary>
    /// Serialize an object to a JSONObject using properties values. It is useful when you
    /// have to send derived or calculated properties. It is not a simple serialization, it bring
    /// also all the logic applyed to the oebjsct properties (es. Price,Q.ty, Discount, Total. Total is
    /// a derived property)
    /// </summary>
    class function ObjectToJSONObject(AObject: TObject): TJSONObject; overload;
    /// <summary>
    /// Identical to ObjectToJSONObject but it return a string representation instead of a json object
    /// </summary>
    class function ObjectToJSONObjectString(AObject: TObject): string;
    class function ObjectToJSONArray(AObject: TObject): TJSONArray;
    { ***** Daniele Spinetti ***** }
    class function JSONArrayToObjectList(AListOf: TClass;
      AJSONArray: TJSONArray; AInstanceOwner: boolean = True;
      AOwnsChildObjects: boolean = True): TObjectList<TObject>; overload;
    { ***** Daniele Spinetti ***** }
    class procedure JSONArrayToObjectList(AList: IWrappedList; AListOf: TClass;
      AJSONArray: TJSONArray; AInstanceOwner: boolean = True;
      AOwnsChildObjects: boolean = True); overload;
    class function JSONArrayToObjectList<T: class, constructor>
      (AJSONArray: TJSONArray; AInstanceOwner: boolean = True;
      AOwnsChildObjects: boolean = True): TObjectList<T>; overload;
    class procedure JSONArrayToObjectList<T: class, constructor>
      (AList: TObjectList<T>; AJSONArray: TJSONArray;
      AInstanceOwner: boolean = True;
      AOwnsChildObjects: boolean = True); overload;
{$IFDEF USEDBX}
    class procedure ReaderToObject(AReader: TDBXReader; AObject: TObject);
    class procedure ReaderToObjectList<T: class, constructor>
      (AReader: TDBXReader; AObjectList: TObjectList<T>);
    class procedure ReaderToJSONObject(AReader: TDBXReader;
      AJSONObject: TJSONObject; AReaderInstanceOwner: boolean = True);
{$IFEND}
    class procedure DataSetToJSONObject(ADataSet: TDataSet;
      AJSONObject: TJSONObject; ADataSetInstanceOwner: boolean = True;
      AJSONObjectActionProc: TJSONObjectActionProc = nil;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase);
    class procedure JSONObjectToDataSet(AJSONObject: TJSONObject;
      ADataSet: TDataSet; AJSONObjectInstanceOwner: boolean = True); overload;
    class procedure JSONObjectToDataSet(AJSONObject: TJSONObject;
      ADataSet: TDataSet; AIgnoredFields: TArray<string>;
      AJSONObjectInstanceOwner: boolean = True;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    class procedure DataSetToObjectList<T: class, constructor>
      (ADataSet: TDataSet; AObjectList: TObjectList<T>;
      ACloseDataSetAfterScroll: boolean = True);
    class function DataSetToJSONArrayOf<T: class, constructor>
      (ADataSet: TDataSet): TJSONArray;
{$IFDEF USEDBX}
    class procedure ReaderToList<T: class, constructor>(AReader: TDBXReader;
      AList: IWrappedList);
    class procedure ReaderToJSONArray(AReader: TDBXReader;
      AJSONArray: TJSONArray; AReaderInstanceOwner: boolean = True);
{$IFEND}
    class procedure DataSetToJSONArray(ADataSet: TDataSet;
      AJSONArray: TJSONArray; ADataSetInstanceOwner: boolean = True;
      AJSONObjectActionProc: TJSONObjectActionProc = nil;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase
      );
    class procedure JSONArrayToDataSet(AJSONArray: TJSONArray;
      ADataSet: TDataSet; AJSONArrayInstanceOwner: boolean = True); overload;
    class procedure JSONArrayToDataSet(AJSONArray: TJSONArray;
      ADataSet: TDataSet; AIgnoredFields: TArray<string>;
      AJSONArrayInstanceOwner: boolean = True;
      AFieldNamePolicy: TFieldNamePolicy = fpLowerCase); overload;
    // class procedure DataSetRowToXML(ADataSet: TDataSet; Row: IXMLNode;
    // ADataSetInstanceOwner: boolean = True);
    // class procedure DataSetToXML(ADataSet: TDataSet; XMLDocument: String;
    // ADataSetInstanceOwner: boolean = True);
    class function ObjectListToJSONArray<T: class>(AList: TObjectList<T>;
      AOwnsInstance: boolean = false; AForEach: TJSONObjectActionProc = nil)
      : TJSONArray; overload;
    class function ObjectListToJSONArray(AList: IWrappedList;
      AOwnsChildObjects: boolean = True; AForEach: TJSONObjectActionProc = nil)
      : TJSONArray; overload;
    class function ObjectListToJSONArrayFields<T: class>(AList: TObjectList<T>;
      AOwnsInstance: boolean = false; AForEach: TJSONObjectActionProc = nil)
      : TJSONArray; overload;
    class function ObjectListToJSONArrayFields(AList: IWrappedList;
      AOwnsChildObjects: boolean = True; AForEach: TJSONObjectActionProc = nil)
      : TJSONArray; overload;
    class function ObjectListToJSONArrayString<T: class>(AList: TObjectList<T>;
      AOwnsInstance: boolean = false): string; overload;
    class function ObjectListToJSONArrayString(AList: IWrappedList;
      AOwnsChildObjects: boolean = True): string; overload;
    class function ObjectListToJSONArrayOfJSONArray<T: class, constructor>
      (AList: TObjectList<T>): TJSONArray;
    class function GetProperty(Obj: TObject; const PropertyName: string)
      : TValue; static;
{$IFDEF USEDBX}
    class function ExecuteSQLQueryNoResult(AQuery: TSQLQuery;
      AObject: TObject): Int64;
    class procedure ExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject = nil);
    class function ExecuteSQLQueryAsObjectList<T: class, constructor>
      (AQuery: TSQLQuery; AObject: TObject = nil): TObjectList<T>;
    class function CreateQuery(AConnection: TSQLConnection; ASQL: string)
      : TSQLQuery;
{$IFEND}
    { FIREDAC RELATED METHODS }
{$IFDEF USEFIREDAC}
    class function ExecuteFDQueryNoResult(AQuery: TFDQuery;
      AObject: TObject): Int64;
    class procedure ExecuteFDQuery(AQuery: TFDQuery; AObject: TObject);
    class procedure ObjectToFDParameters(AFDParams: TFDParams; AObject: TObject;
      AParamPrefix: string = '');
{$ENDIF}
    // SAFE TJSONObject getter
    class function GetPair(JSONObject: TJSONObject; PropertyName: string)
      : TJSONPair;
    class function GetStringDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: string = ''): string;
    class function GetNumberDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Extended = 0): Extended;
    class function GetJSONObj(JSONObject: TJSONObject; PropertyName: string)
      : TJSONObject;
    class function GetJSONArray(JSONObject: TJSONObject; PropertyName: string)
      : TJSONArray;
    class function GetIntegerDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Integer = 0): Integer;
    class function GetInt64Def(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Int64 = 0): Int64;
    class function GetBooleanDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: boolean = false): boolean;
    class function PropertyExists(JSONObject: TJSONObject;
      PropertyName: string): boolean;
  end;

  MapperTransientAttribute = class(TCustomAttribute)

  end;

  DoNotSerializeAttribute = MVCDoNotSerializeAttribute;

  MapperItemsClassType = class(TCustomAttribute)
  private
    FValue: TClass;
    procedure SetValue(const Value: TClass);

  public
    constructor Create(Value: TClass);
    property Value: TClass read FValue write SetValue;
  end;

  MapperListOf = MapperItemsClassType; // just to be more similar to DORM

  TJSONNameCase = (JSONNameUpperCase, JSONNameLowerCase);

  HideInGrids = class(TCustomAttribute)

  end;

  StringValueAttribute = class abstract(TCustomAttribute)
  private
    FValue: string;
    procedure SetValue(const Value: string);

  public
    constructor Create(Value: string);
    property Value: string read FValue write SetValue;
  end;

  FormatFloatValue = class(StringValueAttribute)

  end;

  FormatDateTimeValue = class(StringValueAttribute)

  end;

 // MapperSerializeAsString = MVCSerializeAsStringAttribute;
  MapperSerializeAsString = class(TCustomAttribute)
  strict private
    FEncoding: string;
    procedure SetEncoding(const Value: string);
  const
    DefaultEncoding = 'utf8';
  private
    function GetEncoding: string;
  public
    constructor Create(aEncoding: string = DefaultEncoding);
    property Encoding: string read GetEncoding write SetEncoding;
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

  MapperColumnAttribute_DEPRECATED = class(TCustomAttribute)
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

function ISODateTimeToString(ADateTime: TDateTime): string;
function ISODateToString(ADate: TDateTime): string;
function ISOTimeToString(ATime: TTime): string;

function ISOStrToDateTime(const DateTimeAsString: string): TDateTime;
function ISOStrToDate(const DateAsString: string): TDate;
function ISOStrToTime(const TimeAsString: string): TTime;


// function ISODateToStr(const ADate: TDate): String;
//
// function ISOTimeToStr(const ATime: TTime): String;

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses
  TypInfo,
  FmtBcd,
  Math,
  SqlTimSt,
  DateUtils,
  MVCFramework.Rtti.Utils,
  Xml.adomxmldom,
{$IFDEF SYSTEMNETENCODING}
  System.NetEncoding,
  // so that the old functions in Soap.EncdDecd can be inlined
{$ENDIF}
  Soap.EncdDecd;

{ Mapper }

function ContainsFieldName(const FieldName: string;
  var FieldsArray: TArray<string>): boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FieldsArray) - 1 do
  begin
    if SameText(FieldsArray[I], FieldName) then
      Exit(True);
  end;
  Result := false;
end;

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
  fs.DateSeparator := '-';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function CheckISOTimeStrSeparator(const TimeAsString: string; const Offset: Word): boolean;
begin
  Result := (TimeAsString.Chars[Offset + 2] = ':') and
    (TimeAsString.Chars[Offset + 5] = ':');
end;

function CheckISODateStrSeparator(const DateAsString: string; const Offset: Word): boolean;
begin
  Result := (DateAsString.Chars[Offset + 4] = '-') and
    (DateAsString.Chars[Offset + 7] = '-');
end;

function ISOStrToDateTime(const DateTimeAsString: string): TDateTime;
begin
  if not CheckISODateStrSeparator(DateTimeAsString, 0) then
    raise EMapperException.Create('Invalid ISO DateTime String');

  if not CheckISOTimeStrSeparator(DateTimeAsString, 11) then
    raise EMapperException.Create('Invalid ISO DateTime String');

  Result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function ISOStrToTime(const TimeAsString: string): TTime;
begin
  if not CheckISOTimeStrSeparator(TimeAsString, 0) then
    raise EMapperException.Create('Invalid ISO Time String');

  Result := EncodeTime(StrToInt(Copy(TimeAsString, 1, 2)),
    StrToInt(Copy(TimeAsString, 4, 2)),
    StrToIntDef(Copy(TimeAsString, 7, 2), 0), 0);
end;

function ISOStrToDate(const DateAsString: string): TDate;
begin
  if not CheckISODateStrSeparator(DateAsString, 0) then
    raise EMapperException.Create('Invalid ISO Date String');

  Result := EncodeDate(StrToInt(Copy(DateAsString, 1, 4)),
    StrToInt(Copy(DateAsString, 6, 2)), StrToInt(Copy(DateAsString, 9, 2)));
end;

{$IFDEF USEDBX}


class function Mapper.InternalExecuteSQLQuery(AQuery: TSQLQuery;
  AObject: TObject; WithResult: boolean): Int64;
var
  I: Integer;
  pname: string;
  _rttiType: TRttiType;
  obj_fields: TArray<TRttiProperty>;
  obj_field: TRttiProperty;
  obj_field_attr: MapperColumnAttribute_DEPRECATED;
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
        if HasAttribute<MapperColumnAttribute_DEPRECATED>(obj_field, obj_field_attr) then
        begin
          Map.Add(MapperColumnAttribute_DEPRECATED(obj_field_attr).FieldName, obj_field);
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

class procedure Mapper.ReaderToJSONArray(AReader: TDBXReader;
  AJSONArray: TJSONArray; AReaderInstanceOwner: boolean);
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

class procedure Mapper.ReaderToJSONObject(AReader: TDBXReader;
  AJSONObject: TJSONObject; AReaderInstanceOwner: boolean);
var
  I: Integer;
  key: string;
  dt: TDateTime;
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
        AJSONObject.AddPair(key,
          TJSONNumber.Create(BcdToDouble(AReader.Value[I].AsBcd)));
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
      raise EMapperException.Create('Cannot find type');
    end;
  end;
  if AReaderInstanceOwner then
    FreeAndNil(AReader);
end;

class procedure Mapper.ReaderToList<T>(AReader: TDBXReader;
  AList: IWrappedList);
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
  _attribute: MapperColumnAttribute_DEPRECATED;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MapperColumnAttribute_DEPRECATED;
  field_name: string;
  Value: TValue;
  ts: TTimeStamp;
  sqlts: TSQLTimeStamp;
begin
  _dict := TDictionary<string, string>.Create();
  _keys := TDictionary<string, boolean>.Create();
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
    if HasAttribute<MapperColumnAttribute_DEPRECATED>(_field, _attribute) then
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
    if (not _dict.TryGetValue(_field.Name, field_name)) or
      (not _field.IsWritable) or (HasAttribute<MapperTransientAttribute>(_field))
    then
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
            if AReader.Value[field_name].ValueType.DataType = TDBXDataTypes.DateType
            then
            begin
              ts.Time := 0;
              ts.date := AReader.Value[field_name].AsDate;
              Value := TimeStampToDateTime(ts);
            end
            else if AReader.Value[field_name]
              .ValueType.DataType = TDBXDataTypes.DoubleType then
              Value := AReader.Value[field_name].AsDouble
            else if AReader.Value[field_name]
              .ValueType.DataType = TDBXDataTypes.BcdType then
              Value := BcdToDouble(AReader.Value[field_name].AsBcd)
            else if AReader.Value[field_name]
              .ValueType.DataType = TDBXDataTypes.TimeType then
            begin
              sqlts := AReader.Value[field_name].AsTimeStamp;
              Value := SQLTimeStampToDateTime(sqlts);
            end
            else
              raise EMapperException.Create('Unknown tkFloat Type');
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
      raise EMapperException.Create('Unknown field type for ' + field_name);
    end;
    _field.SetValue(AObject, Value);
  end;
  _dict.Free;
  _keys.Free;
end;

class procedure Mapper.ReaderToObjectList<T>(AReader: TDBXReader;
  AObjectList: TObjectList<T>);
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

class function Mapper.CreateQuery(AConnection: TSQLConnection; ASQL: string)
  : TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.SQLConnection := AConnection;
  Result.CommandText := ASQL;
end;
{$IFEND}


class procedure Mapper.DataSetToJSONArray(ADataSet: TDataSet;
  AJSONArray: TJSONArray; ADataSetInstanceOwner: boolean;
  AJSONObjectActionProc: TJSONObjectActionProc;
  AFieldNamePolicy: TFieldNamePolicy);
var
  Obj: TJSONObject;
begin
  while not ADataSet.Eof do
  begin
    Obj := TJSONObject.Create;
    AJSONArray.AddElement(Obj);
    DataSetToJSONObject(ADataSet, Obj, false, AJSONObjectActionProc, AFieldNamePolicy);
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

class procedure Mapper.DataSetToJSONObject(ADataSet: TDataSet;
  AJSONObject: TJSONObject; ADataSetInstanceOwner: boolean;
  AJSONObjectActionProc: TJSONObjectActionProc;
  AFieldNamePolicy: TFieldNamePolicy);
var
  I: Integer;
  key: string;
  ts: TSQLTimeStamp;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    // Name policy { ***** Daniele Spinetti ***** }
    case AFieldNamePolicy of
      fpLowerCase:
        key := LowerCase(ADataSet.Fields[I].FieldName);
      fpUpperCase:
        key := UpperCase(ADataSet.Fields[I].FieldName);
      fpAsIs:
        key := ADataSet.Fields[I].FieldName;
    end;

    if ADataSet.Fields[I].IsNull then
    begin
      AJSONObject.AddPair(key, TJSONNull.Create);
      Continue;
    end;
    case ADataSet.Fields[I].DataType of
      TFieldType.ftInteger, TFieldType.ftLongWord, TFieldType.ftAutoInc, TFieldType.ftSmallint,
        TFieldType.ftShortint:
        AJSONObject.AddPair(key,
          TJSONNumber.Create(ADataSet.Fields[I].AsInteger));
      TFieldType.ftLargeint:
        begin
          AJSONObject.AddPair(key,
            TJSONNumber.Create(ADataSet.Fields[I].AsLargeInt));
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        AJSONObject.AddPair(key,
          TJSONNumber.Create(ADataSet.Fields[I].AsFloat));
      ftWideString, ftMemo, ftWideMemo:
        AJSONObject.AddPair(key, ADataSet.Fields[I].AsWideString);
      ftString:
        AJSONObject.AddPair(key, ADataSet.Fields[I].AsString);
      TFieldType.ftDate:
        AJSONObject.AddPair(key,
          ISODateToString(ADataSet.Fields[I].AsDateTime));
      TFieldType.ftDateTime:
        AJSONObject.AddPair(key,
          ISODateTimeToString(ADataSet.Fields[I].AsDateTime));
      TFieldType.ftTimeStamp:
        begin
          ts := ADataSet.Fields[I].AsSQLTimeStamp;
          AJSONObject.AddPair(key,
            SQLTimeStampToStr('yyyy-mm-dd hh:nn:ss', ts));
        end;
      TFieldType.ftCurrency:
        AJSONObject.AddPair(key,
          TJSONNumber.Create(ADataSet.Fields[I].AsCurrency));
      TFieldType.ftBCD, TFieldType.ftFMTBcd:
        AJSONObject.AddPair(key,
          TJSONNumber.Create(BcdToDouble(ADataSet.Fields[I].AsBcd)));
      TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            TBlobField(ADataSet.Fields[I]).SaveToStream(MS);
            MS.Position := 0;
            SS := TStringStream.Create('', TEncoding.ASCII);
            try
              EncodeStream(MS, SS);
              SS.Position := 0;
              AJSONObject.AddPair(key, SS.DataString);
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      TFieldType.ftBoolean:
        begin
          if ADataSet.Fields[I].AsBoolean then
            AJSONObject.AddPair(key, TJSONTrue.Create)
          else
            AJSONObject.AddPair(key, TJSONFalse.Create);
        end;

      // else
      // raise EMapperException.Create('Cannot find type for field ' + key);
    end;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
  if Assigned(AJSONObjectActionProc) then
    AJSONObjectActionProc(AJSONObject);
end;

class procedure Mapper.DataSetToObject(ADataSet: TDataSet; AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  _attribute: TCustomAttribute;
  _dict: TDictionary<string, string>;
  _keys: TDictionary<string, boolean>;
  mf: MapperColumnAttribute_DEPRECATED;
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
      if _attribute is MapperColumnAttribute_DEPRECATED then
      begin
        FoundAttribute := True;
        mf := MapperColumnAttribute_DEPRECATED(_attribute);
        _dict.Add(_field.Name, mf.FieldName);
        _keys.Add(_field.Name, mf.IsPK);
      end
      else if _attribute is MapperTransientAttribute then
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

class function Mapper.ObjectListToJSONArrayFields(AList: IWrappedList;
  AOwnsChildObjects: boolean = True; AForEach: TJSONObjectActionProc = nil)
  : TJSONArray;
var
  I: Integer;
  JV: TJSONObject;
begin
  Result := TJSONArray.Create;
  AList.OwnsObjects := AOwnsChildObjects;
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
    begin
      JV := ObjectToJSONObjectFields(AList.GetItem(I), []);
      if Assigned(AForEach) then
        AForEach(JV);
      Result.AddElement(JV);
    end;
end;

class function Mapper.ObjectListToJSONArrayFields<T>(AList: TObjectList<T>;
  AOwnsInstance: boolean = false; AForEach: TJSONObjectActionProc = nil)
  : TJSONArray;
var
  I: Integer;
  JV: TJSONObject;
begin
  Result := TJSONArray.Create;
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
    begin
      JV := ObjectToJSONObjectFields(AList[I], []);
      if Assigned(AForEach) then
        AForEach(JV);
      Result.AddElement(JV);
    end;
  if AOwnsInstance then
    AList.Free;
end;

class function Mapper.ObjectListToJSONArray<T>(AList: TObjectList<T>;
  AOwnsInstance: boolean; AForEach: TJSONObjectActionProc): TJSONArray;
var
  I: Integer;
  JV: TJSONObject;
begin
  Result := TJSONArray.Create;
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
    begin
      JV := ObjectToJSONObject(AList[I]);
      if Assigned(AForEach) then
        AForEach(JV);
      Result.AddElement(JV);
    end;
  if AOwnsInstance then
    AList.Free;
end;

class function Mapper.ObjectListToJSONArray(AList: IWrappedList;
  AOwnsChildObjects: boolean; AForEach: TJSONObjectActionProc): TJSONArray;
var
  I: Integer;
  JV: TJSONObject;
begin
  Result := TJSONArray.Create;
  if Assigned(AList) then
  begin
    AList.OwnsObjects := AOwnsChildObjects;
    for I := 0 to AList.Count - 1 do
    begin
      JV := ObjectToJSONObject(AList.GetItem(I));
      if Assigned(AForEach) then
        AForEach(JV);
      Result.AddElement(JV);
    end;
  end;
end;

class function Mapper.ObjectListToJSONArrayOfJSONArray<T>(AList: TObjectList<T>)
  : TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to AList.Count - 1 do
    Result.AddElement(ObjectToJSONArray(AList[I]));
end;

class function Mapper.ObjectListToJSONArrayString<T>(AList: TObjectList<T>;
  AOwnsInstance: boolean): string;
var
  Arr: TJSONArray;
begin
  Arr := Mapper.ObjectListToJSONArray<T>(AList, AOwnsInstance);
  try
    Result := Arr.ToString;
  finally
    Arr.Free;
  end;
end;

class function Mapper.ObjectListToJSONArrayString(AList: IWrappedList;
  AOwnsChildObjects: boolean): string;
var
  Arr: TJSONArray;
begin
  Arr := Mapper.ObjectListToJSONArray(AList, AOwnsChildObjects);
  try
    Result := Arr.ToString;
  finally
    Arr.Free;
  end;
end;

class procedure Mapper.ObjectToDataSet(Obj: TObject; Field: TField;
  var Value: Variant);
begin
  Value := GetProperty(Obj, Field.FieldName).AsVariant;
end;

class function Mapper.ObjectToJSONArray(AObject: TObject): TJSONArray;
var
  LRTTIType: TRttiType;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
  LKeyName: string;
  LJArray: TJSONArray;
  LObj: TObject;
  LList: IWrappedList;
  LJArr: TJSONArray;
  LObjItem: TObject;
begin
  LJArray := TJSONArray.Create;
  LRTTIType := ctx.GetType(AObject.ClassInfo);
  LProperties := LRTTIType.GetProperties;
  for LProperty in LProperties do
  begin
    if HasAttribute<MVCDoNotSerializeAttribute>(LProperty) then
      Continue;
    LKeyName := GetKeyName(LProperty, LRTTIType);
    case LProperty.PropertyType.TypeKind of
      tkEnumeration:
        begin
          LJArray.AddElement(SerializeEnumerationProperty(AObject, LProperty));
          // if LProperty.PropertyType.QualifiedName = 'System.Boolean' then
          // begin
          // if LProperty.GetValue(AObject).AsBoolean then
          // LJArray.AddElement(TJSONTrue.Create)
          // else
          // LJArray.AddElement(TJSONFalse.Create)
          // end;
        end;
      tkInteger, tkInt64:
        LJArray.AddElement(TJSONNumber.Create(LProperty.GetValue(AObject)
          .AsInteger));
      tkFloat:
        begin
          LJArray.AddElement(SerializeFloatProperty(AObject, LProperty));
        end;
      tkString, tkLString, tkWString, tkUString:
        LJArray.AddElement(TJSONString.Create(LProperty.GetValue(AObject)
          .AsString));
      tkClass:
        begin
          LObj := LProperty.GetValue(AObject).AsObject;
          if Assigned(LObj) then
          begin
            LList := nil;
            if TDuckTypedList.CanBeWrappedAsList(LObj) then
              LList := WrapAsList(LObj);
            if Assigned(LList) then
            begin
              LJArr := TJSONArray.Create;
              LJArray.AddElement(LJArr);
              for LObjItem in LList do
              begin
                LJArr.AddElement(ObjectToJSONObject(LObjItem));
              end;
            end
            else
            begin
              LJArray.AddElement(ObjectToJSONObject(LProperty.GetValue(AObject)
                .AsObject));
            end;
          end
          else
            LJArray.AddElement(TJSONNull.Create);
        end;
    end;
  end;
  Result := LJArray;
end;

class function Mapper.ObjectToJSONObject(AObject: TObject;
  AIgnoredProperties: array of string): TJSONObject;
var
  _type: TRttiType;
  _properties: TArray<TRttiProperty>;
  _property: TRttiProperty;
  f: string;
  JSONObject: TJSONObject;
  Arr: TJSONArray;
  list: IWrappedList;
  Obj, o: TObject;
  DoNotSerializeThis: boolean;
  I: Integer;
  ThereAreIgnoredProperties: boolean;
  ts: TTimeStamp;
  sr: TStringStream;
  SS: TStringStream;
  _attrser: MapperSerializeAsString;
  SerEnc: TEncoding;
  // attr: MapperItemsClassType;
  // ListCount: Integer;
  // ListItems: TRttiMethod;
  // ListItemValue: TValue;
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

    if HasAttribute<MVCDoNotSerializeAttribute>(_property) then
      Continue;

    case _property.PropertyType.TypeKind of
      tkInteger, tkInt64:
        JSONObject.AddPair(f, TJSONNumber.Create(_property.GetValue(AObject)
          .AsInteger));
      tkFloat:
        begin
          JSONObject.AddPair(f, SerializeFloatProperty(AObject, _property));
          {
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
          }
        end;
      tkString, tkLString, tkWString, tkUString:
        JSONObject.AddPair(f, _property.GetValue(AObject).AsString);
      tkEnumeration:
        begin
          JSONObject.AddPair(f, SerializeEnumerationProperty(AObject,
            _property));
          // if _property.PropertyType.QualifiedName = 'System.Boolean' then
          // begin
          // if _property.GetValue(AObject).AsBoolean then
          // JSONObject.AddPair(f, TJSONTrue.Create)
          // else
          // JSONObject.AddPair(f, TJSONFalse.Create);
          // end
          // else
          // begin
          // JSONObject.AddPair(f, TJSONNumber.Create(_property.GetValue(AObject).AsOrdinal));
          // end;
        end;
      tkRecord:
        begin
          if _property.PropertyType.QualifiedName = 'System.SysUtils.TTimeStamp'
          then
          begin
            ts := _property.GetValue(AObject)
              .AsType<System.SysUtils.TTimeStamp>;
            JSONObject.AddPair(f, TJSONNumber.Create(TimeStampToMsecs(ts)));
          end;
        end;
      tkClass:
        begin
          o := _property.GetValue(AObject).AsObject;
          if Assigned(o) then
          begin
            if TDuckTypedList.CanBeWrappedAsList(o) then
            begin
              if True { Mapper.HasAttribute<MapperItemsClassType>(_property, attr) or
                Mapper.HasAttribute<MapperItemsClassType>
                (_property.PropertyType, attr) } then
              begin
                list := WrapAsList(o);
                if Assigned(list) then
                begin
                  Arr := TJSONArray.Create;
                  JSONObject.AddPair(f, Arr);
                  for Obj in list do
                    if Assigned(Obj) then
                      // nil element into the list are not serialized
                      Arr.AddElement(ObjectToJSONObject(Obj));
                end;
              end
              // else // Ezequiel J. Müller convert regular list
              // begin
              // ListCount := ctx.GetType(o.ClassInfo).GetProperty('Count')
              // .GetValue(o).AsInteger;
              // ListItems := ctx.GetType(o.ClassInfo)
              // .GetIndexedProperty('Items').ReadMethod;
              // if (ListCount > 0) and (ListItems <> nil) then
              // begin
              // Arr := TJSONArray.Create;
              // JSONObject.AddPair(f, Arr);
              // for I := 0 to ListCount - 1 do
              // begin
              // ListItemValue := ListItems.Invoke(o, [I]);
              // case ListItemValue.TypeInfo.Kind of
              // tkInteger:
              // Arr.AddElement
              // (TJSONNumber.Create(ListItemValue.AsInteger));
              // tkInt64:
              // Arr.AddElement
              // (TJSONNumber.Create(ListItemValue.AsInt64));
              // tkFloat:
              // Arr.AddElement
              // (TJSONNumber.Create(ListItemValue.AsExtended));
              // tkString, tkLString, tkWString, tkUString:
              // Arr.AddElement
              // (TJSONString.Create(ListItemValue.AsString));
              // end;
              // end;
              // end;
              // end;
            end
            else if o is TStream then
            begin
              if HasAttribute<MapperSerializeAsString>(_property, _attrser) then
              begin
                // serialize the stream as a normal string...
                TStream(o).Position := 0;
                SerEnc := TEncoding.GetEncoding(_attrser.Encoding);
                sr := TStringStream.Create('', SerEnc);
                try
                  sr.LoadFromStream(TStream(o));
                  JSONObject.AddPair(f, sr.DataString);
                finally
                  sr.Free;
                end;
              end
              else
              begin
                // serialize the stream as Base64 encoded string...
                TStream(o).Position := 0;
                SS := TStringStream.Create;
                try
                  EncodeStream(TStream(o), SS);
                  JSONObject.AddPair(f, SS.DataString);
                finally
                  SS.Free;
                end;
              end;
            end
            else
            begin
              JSONObject.AddPair(f,
                ObjectToJSONObject(_property.GetValue(AObject).AsObject));
            end;
          end
          else
          begin
            if HasAttribute<MapperSerializeAsString>(_property) then
              JSONObject.AddPair(f, '')
            else
              JSONObject.AddPair(f, TJSONNull.Create);
          end;
        end;
    end;
  end;
  Result := JSONObject;
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
  Arr: TJSONArray;
  list: IWrappedList;
  Obj, o: TObject;
  DoNotSerializeThis: boolean;
  I: Integer;
  ThereAreIgnoredProperties: boolean;
  JObj: TJSONObject;
begin
  ThereAreIgnoredProperties := Length(AIgnoredProperties) > 0;
  JSONObject := TJSONObject.Create;
  try
    // add the $dmvc.classname property to allows a strict deserialization
    JSONObject.AddPair(DMVC_CLASSNAME, AObject.QualifiedClassName);
    _type := ctx.GetType(AObject.ClassInfo);
    _fields := _type.GetFields;
    for _field in _fields do
    begin
      f := GetKeyName(_field, _type);
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
          JSONObject.AddPair(f, TJSONNumber.Create(_field.GetValue(AObject)
            .AsInteger));
        tkFloat:
          begin
            JSONObject.AddPair(f, SerializeFloatField(AObject, _field));
          end;
        tkString, tkLString, tkWString, tkUString:
          JSONObject.AddPair(f, _field.GetValue(AObject).AsString);
        tkEnumeration:
          begin
            JSONObject.AddPair(f, SerializeEnumerationField(AObject, _field));
          end;
        tkClass:
          begin
            o := _field.GetValue(AObject).AsObject;
            if Assigned(o) then
            begin
              if TDuckTypedList.CanBeWrappedAsList(o) then
              begin
                list := WrapAsList(o);
                JObj := TJSONObject.Create;
                JSONObject.AddPair(f, JObj);
                JObj.AddPair(DMVC_CLASSNAME, o.QualifiedClassName);
                Arr := TJSONArray.Create;
                JObj.AddPair('items', Arr);
                for Obj in list do
                begin
                  Arr.AddElement(ObjectToJSONObjectFields(Obj, []));
                end;
              end
              else
              begin
                JSONObject.AddPair(f,
                  ObjectToJSONObjectFields(_field.GetValue(AObject)
                  .AsObject, []));
              end;
            end
            else
              JSONObject.AddPair(f, TJSONNull.Create);
          end;
      end;
    end;
    Result := JSONObject;
  except
    FreeAndNil(JSONObject);
    raise;
  end;
end;

class function Mapper.ObjectToJSONObjectFieldsString(AObject: TObject;
  AIgnoredProperties: array of string): string;
var
  LJObj: TJSONObject;
begin
  LJObj := ObjectToJSONObjectFields(AObject, AIgnoredProperties);
  try
    { .$IFDEF TOJSON }
    Result := LJObj.ToJSON;
    { .$ELSE }
    // Result := LJObj.ToString
    { .$IFEND }
  finally
    LJObj.Free;
  end;
end;

class function Mapper.ObjectToJSONObjectString(AObject: TObject): string;
var
  JObj: TJSONObject;
begin
  JObj := ObjectToJSONObject(AObject);
  try
    Result := JObj.ToString;
  finally
    JObj.Free;
  end;
end;

class function Mapper.PropertyExists(JSONObject: TJSONObject;
  PropertyName: string): boolean;
begin
  Result := Assigned(GetPair(JSONObject, PropertyName));
end;

class function Mapper.SerializeEnumerationField(AObject: TObject;
  ARttiField: TRttiField): TJSONValue;
begin
  if ARttiField.FieldType.QualifiedName = 'System.Boolean' then
  begin
    if ARttiField.GetValue(AObject).AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
  begin
    Result := TJSONNumber.Create(ARttiField.GetValue(AObject).AsOrdinal);
  end;
end;

class function Mapper.SerializeEnumerationProperty(AObject: TObject;
  ARTTIProperty: TRttiProperty): TJSONValue;
begin
  if ARTTIProperty.PropertyType.QualifiedName = 'System.Boolean' then
  begin
    if ARTTIProperty.GetValue(AObject).AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
  begin
    Result := TJSONNumber.Create(ARTTIProperty.GetValue(AObject).AsOrdinal);
  end;
end;

class function Mapper.SerializeFloatField(AObject: TObject;
  ARttiField: TRttiField): TJSONValue;
begin
  if ARttiField.FieldType.QualifiedName = 'System.TDate' then
  begin
    if ARttiField.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create(ISODateToString(ARttiField.GetValue(AObject)
        .AsExtended))
  end
  else if ARttiField.FieldType.QualifiedName = 'System.TDateTime' then
  begin
    if ARttiField.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateTimeToString(ARttiField.GetValue(AObject).AsExtended))
  end
  else if ARttiField.FieldType.QualifiedName = 'System.TTime' then
    Result := TJSONString.Create(ISOTimeToString(ARttiField.GetValue(AObject)
      .AsExtended))
  else
    Result := TJSONNumber.Create(ARttiField.GetValue(AObject).AsExtended);
end;

class function Mapper.SerializeFloatProperty(AObject: TObject;
  ARTTIProperty: TRttiProperty): TJSONValue;
begin
  if ARTTIProperty.PropertyType.QualifiedName = 'System.TDate' then
  begin
    if ARTTIProperty.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateToString(ARTTIProperty.GetValue(AObject).AsExtended))
  end
  else if ARTTIProperty.PropertyType.QualifiedName = 'System.TDateTime' then
  begin
    if ARTTIProperty.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateTimeToString(ARTTIProperty.GetValue(AObject).AsExtended))
  end
  else if ARTTIProperty.PropertyType.QualifiedName = 'System.TTime' then
    Result := TJSONString.Create(ISOTimeToString(ARTTIProperty.GetValue(AObject)
      .AsExtended))
  else
    Result := TJSONNumber.Create(ARTTIProperty.GetValue(AObject).AsExtended);

  // if ARTTIProperty.PropertyType.QualifiedName = 'System.TDate' then
  // Result := TJSONString.Create(ISODateToString(ARTTIProperty.GetValue(AObject).AsExtended))
  // else if ARTTIProperty.PropertyType.QualifiedName = 'System.TDateTime' then
  // Result := TJSONString.Create(ISODateTimeToString(ARTTIProperty.GetValue(AObject).AsExtended))
  // else if ARTTIProperty.PropertyType.QualifiedName = 'System.TTime' then
  // Result := TJSONString.Create(ISOTimeToString(ARTTIProperty.GetValue(AObject).AsExtended))
  // else
  // Result := TJSONNumber.Create(ARTTIProperty.GetValue(AObject).AsExtended);
end;

class function Mapper.GetKeyName(const ARttiField: TRttiField;
  AType: TRttiType): string;
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
    raise EMapperException.CreateFmt('Property %s is not a Boolean Property',
      [PropertyName]);
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
    raise EMapperException.CreateFmt('Property %s is not a Int64 Property',
      [PropertyName]);
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
    raise EMapperException.CreateFmt('Property %s is not an Integer Property',
      [PropertyName]);

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
    raise EMapperException.Create('Property is not a JSONArray');

end;

class function Mapper.GetJSONObj(JSONObject: TJSONObject; PropertyName: string)
  : TJSONObject;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(nil);
  if pair.JsonValue is TJSONObject then
    Exit(TJSONObject(pair.JsonValue))
  else
    raise EMapperException.Create('Property is not a JSONObject');
end;

class function Mapper.GetKeyName(const ARttiProp: TRttiProperty;
  AType: TRttiType): string;
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
    raise EMapperException.Create('Property is not a Number Property');
end;

class function Mapper.GetPair(JSONObject: TJSONObject; PropertyName: string)
  : TJSONPair;
var
  pair: TJSONPair;
begin
  if not Assigned(JSONObject) then
    raise EMapperException.Create('JSONObject is nil');
  pair := JSONObject.Get(PropertyName);
  Result := pair;
end;

class function Mapper.GetProperty(Obj: TObject;
  const PropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARTTIType: TRttiType;
begin
  ARTTIType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARTTIType) then
    raise EMapperException.CreateFmt('Cannot get RTTI for type [%s]',
      [ARTTIType.ToString]);
  Prop := ARTTIType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise EMapperException.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(Obj)
  else
    raise EMapperException.CreateFmt('Property is not readable [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
end;

class function Mapper.GetStringDef(JSONObject: TJSONObject;
  PropertyName, DefaultValue: string): string;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONString then
    Exit(TJSONString(pair.JsonValue).Value)
  else
    raise EMapperException.Create('Property is not a String Property');
end;

class function Mapper.HasAttribute<T>(ARTTIMember: TRttiNamedObject;
  out AAttribute: T): boolean;
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

class function Mapper.HasAttribute<T>(ARTTIMember: TRttiNamedObject): boolean;
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

class procedure Mapper.JSONArrayToDataSet(AJSONArray: TJSONArray;
  ADataSet: TDataSet; AJSONArrayInstanceOwner: boolean);
begin
  JSONArrayToDataSet(AJSONArray, ADataSet, TArray<string>.Create(),
    AJSONArrayInstanceOwner);
end;

class procedure Mapper.JSONArrayToDataSet(AJSONArray: TJSONArray;
  ADataSet: TDataSet; AIgnoredFields: TArray<string>;
  AJSONArrayInstanceOwner: boolean; AFieldNamePolicy: TFieldNamePolicy);
var
  I: Integer;
begin
  for I := 0 to AJSONArray.Size - 1 do
  begin
    ADataSet.Append;
    Mapper.JSONObjectToDataSet(AJSONArray.Get(I) as TJSONObject, ADataSet,
      AIgnoredFields, false, AFieldNamePolicy);
    ADataSet.Post;
  end;
  if AJSONArrayInstanceOwner then
    AJSONArray.Free;
end;

class function Mapper.JSONArrayToObjectList(AListOf: TClass;
  AJSONArray: TJSONArray; AInstanceOwner: boolean = True;
  AOwnsChildObjects: boolean = True): TObjectList<TObject>;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(AJSONArray) then
  begin
    Result := TObjectList<TObject>.Create(AOwnsChildObjects);
    for I := 0 to AJSONArray.Size - 1 do
      Result.Add(Mapper.JSONObjectToObject(AListOf,
        AJSONArray.Get(I) as TJSONObject));
    if AInstanceOwner then
      AJSONArray.Free;
  end;
end;

class procedure Mapper.JSONArrayToObjectList(AList: IWrappedList;
  AListOf: TClass; AJSONArray: TJSONArray; AInstanceOwner: boolean = True;
  AOwnsChildObjects: boolean = True);
var
  I: Integer;
begin
  if Assigned(AJSONArray) then
  begin
    AList.OwnsObjects := AOwnsChildObjects;
    for I := 0 to AJSONArray.Size - 1 do
      AList.Add(Mapper.JSONObjectToObject(AListOf,
        AJSONArray.Get(I) as TJSONObject));
    if AInstanceOwner then
      AJSONArray.Free;
  end;
end;

class procedure Mapper.JSONArrayToObjectList<T>(AList: TObjectList<T>;
  AJSONArray: TJSONArray; AInstanceOwner, AOwnsChildObjects: boolean);
var
  I: Integer;
begin
  if Assigned(AJSONArray) then
  begin
    for I := 0 to AJSONArray.Size - 1 do
      AList.Add(Mapper.JSONObjectToObject<T>(AJSONArray.Get(I) as TJSONObject));
    if AInstanceOwner then
      AJSONArray.Free;
  end;
end;

class function Mapper.JSONArrayToObjectList<T>(AJSONArray: TJSONArray;
  AInstanceOwner: boolean; AOwnsChildObjects: boolean): TObjectList<T>;
begin
  Result := TObjectList<T>.Create(AOwnsChildObjects);
  JSONArrayToObjectList<T>(Result, AJSONArray, AInstanceOwner,
    AOwnsChildObjects);
end;

class procedure Mapper.InternalJSONObjectFieldsToObject(ctx: TRTTIContext;
  AJSONObject: TJSONObject; AObject: TObject);
  procedure RaiseExceptForField(FieldName: string);
  begin
    raise EMapperException.Create
      (FieldName + ' key field is not present in the JSONObject');
  end;

var
  _type: TRttiType;
  _fields: TArray<TRttiField>;
  _field: TRttiField;
  f: string;
  jvalue: TJSONValue;
  v: TValue;
  o: TObject;
  list: IWrappedList;
  I: Integer;
  Arr: TJSONArray;
  n: TJSONNumber;
  SerStreamASString: string;
  sw: TStreamWriter;
  SS: TStringStream;
  _attrser: MapperSerializeAsString;
  SerEnc: TEncoding;
  LClassName: string;
  LJSONKeyIsNotPresent: boolean;
begin
  if not Assigned(AJSONObject) then
    raise EMapperException.Create('JSON Object cannot be nil');
  jvalue := nil;
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetFields;
  for _field in _fields do
  begin
    if HasAttribute<MapperTransientAttribute>(_field) then
      Continue;
    f := GetKeyName(_field, _type);
    if Assigned(AJSONObject.Get(f)) then
    begin
      LJSONKeyIsNotPresent := false;
      jvalue := AJSONObject.Get(f).JsonValue;
    end
    else
    begin
      LJSONKeyIsNotPresent := True;
    end;

    case _field.FieldType.TypeKind of
      tkEnumeration:
        begin
          if LJSONKeyIsNotPresent then
            RaiseExceptForField(_field.Name);
          if _field.FieldType.QualifiedName = 'System.Boolean' then
          begin
            if jvalue is TJSONTrue then
              _field.SetValue(TObject(AObject), True)
            else if jvalue is TJSONFalse then
              _field.SetValue(TObject(AObject), false)
            else
              raise EMapperException.Create('Invalid value for property ' +
                _field.Name);
          end
          else // it is an enumerated value but it's not a boolean.
          begin
            TValue.Make((jvalue as TJSONNumber).AsInt,
              _field.FieldType.Handle, v);
            _field.SetValue(TObject(AObject), v);
          end;
        end;
      tkInteger, tkInt64:
        begin
          if LJSONKeyIsNotPresent then
            _field.SetValue(TObject(AObject), 0)
          else
            _field.SetValue(TObject(AObject), StrToIntDef(jvalue.Value, 0));
        end;
      tkFloat:
        begin
          if LJSONKeyIsNotPresent then
          begin
            _field.SetValue(TObject(AObject), 0);
          end
          else
          begin
            if _field.FieldType.QualifiedName = 'System.TDate' then
            begin
              if jvalue is TJSONNull then
                _field.SetValue(TObject(AObject), 0)
              else
                _field.SetValue(TObject(AObject),
                  ISOStrToDateTime(jvalue.Value + ' 00:00:00'))
            end
            else if _field.FieldType.QualifiedName = 'System.TDateTime' then
            begin
              if jvalue is TJSONNull then
                _field.SetValue(TObject(AObject), 0)
              else
                _field.SetValue(TObject(AObject),
                  ISOStrToDateTime(jvalue.Value))
            end
            else if _field.FieldType.QualifiedName = 'System.TTime' then
            begin
              if jvalue is TJSONString then
                _field.SetValue(TObject(AObject), ISOStrToTime(jvalue.Value))
              else
                raise EMapperException.CreateFmt
                  ('Cannot deserialize [%s], expected [%s] got [%s]',
                  [_field.Name, 'TJSONString', jvalue.ClassName]);
            end
            else { if _field.PropertyType.QualifiedName = 'System.Currency' then }
            begin
              if jvalue is TJSONNumber then
                _field.SetValue(TObject(AObject), TJSONNumber(jvalue).AsDouble)
              else
                raise EMapperException.CreateFmt
                  ('Cannot deserialize [%s], expected [%s] got [%s]',
                  [_field.Name, 'TJSONNumber', jvalue.ClassName]);
            end;
          end;
        end;
      tkString, tkLString, tkWString, tkUString:
        begin
          if LJSONKeyIsNotPresent then
            _field.SetValue(TObject(AObject), '')
          else
            _field.SetValue(TObject(AObject), jvalue.Value);
        end;
      tkRecord:
        begin
          if _field.FieldType.QualifiedName = 'System.SysUtils.TTimeStamp' then
          begin
            if LJSONKeyIsNotPresent then
            begin
              _field.SetValue(TObject(AObject),
                TValue.From<TTimeStamp>(MSecsToTimeStamp(0)));
            end
            else
            begin
              n := jvalue as TJSONNumber;
              _field.SetValue(TObject(AObject),
                TValue.From<TTimeStamp>(MSecsToTimeStamp(n.AsInt64)));
            end;
          end;
        end;
      tkClass: // try to restore child properties... but only if the collection is not nil!!!
        begin
          o := _field.GetValue(TObject(AObject)).AsObject;
          if LJSONKeyIsNotPresent then
          begin
            o.Free;
            o := nil;
            _field.SetValue(AObject, nil);
          end;

          if Assigned(o) then
          begin
            if o is TStream then
            begin
              if jvalue is TJSONString then
              begin
                SerStreamASString := TJSONString(jvalue).Value;
              end
              else
                raise EMapperException.Create('Expected JSONString in ' +
                  AJSONObject.Get(f).JsonString.Value);

              if HasAttribute<MapperSerializeAsString>(_field, _attrser) then
              begin
                // serialize the stream as a normal string...
                TStream(o).Position := 0;
                SerEnc := TEncoding.GetEncoding(_attrser.Encoding);
                SS := TStringStream.Create(SerStreamASString, SerEnc);
                try
                  SS.Position := 0;
                  TStream(o).CopyFrom(SS, SS.Size);
                finally
                  SS.Free;
                end;
              end
              else
              begin
                // deserialize the stream as Base64 encoded string...
                TStream(o).Position := 0;
                sw := TStreamWriter.Create(TStream(o));
                try
                  sw.Write(DecodeString(SerStreamASString));
                finally
                  sw.Free;
                end;
              end;
            end
            else if TDuckTypedList.CanBeWrappedAsList(o) then
            begin // restore collection
              if not(jvalue is TJSONObject) then
                raise EMapperException.Create('Wrong serialization for ' +
                  o.QualifiedClassName);
              LClassName := TJSONObject(jvalue).Get(DMVC_CLASSNAME)
                .JsonValue.Value;
              if o = nil then // recreate the object as it should be
              begin
                o := TRTTIUtils.CreateObject(LClassName);
              end;
              jvalue := TJSONObject(jvalue).Get('items').JsonValue;
              if jvalue is TJSONArray then
              begin
                Arr := TJSONArray(jvalue);
                begin
                  list := WrapAsList(o);
                  for I := 0 to Arr.Size - 1 do
                  begin
                    list.Add(Mapper.JSONObjectFieldsToObject(Arr.Get(I)
                      as TJSONObject));
                  end;
                end;
              end
              else
                raise EMapperException.Create('Cannot restore ' + f +
                  ' because the related json property is not an array');
            end
            else // try to deserialize into the property... but the json MUST be an object
            begin
              if jvalue is TJSONObject then
              begin
                InternalJSONObjectFieldsToObject(ctx, TJSONObject(jvalue), o);
              end
              else if jvalue is TJSONNull then
              begin
                FreeAndNil(o);
                _field.SetValue(AObject, nil)
              end
              else
                raise EMapperException.Create('Cannot deserialize property ' +
                  _field.Name);
            end;
          end;
        end;
    end;
  end;
end;

class procedure Mapper.DeSerializeBase64StringStream(aStream: TStream;
  const aBase64SerializedString: string);
var
  SS: TStringStream;
begin
  // deserialize the stream as Base64 encoded string...
  aStream.Size := 0;
  SS := TStringStream.Create(aBase64SerializedString, TEncoding.ASCII);
  try
    SS.Position := 0;
    DecodeStream(SS, aStream);
  finally
    SS.Free;
  end;
end;

class procedure Mapper.DeSerializeStringStream(aStream: TStream;
  const aSerializedString: string; aEncoding: string);
var
  SerEnc: TEncoding;
  SS: TStringStream;
begin
  // deserialize the stream as a normal string...
  aStream.Position := 0;
  SerEnc := TEncoding.GetEncoding(aEncoding);
  SS := TStringStream.Create(aSerializedString, SerEnc);
  try
    SS.Position := 0;
    aStream.CopyFrom(SS, SS.Size);
  finally
    SS.Free;
  end;
end;

class procedure Mapper.InternalJSONObjectToObject(ctx: TRTTIContext;
  AJSONObject: TJSONObject; AObject: TObject);
var
  _type: TRttiType;
  _fields: TArray<TRttiProperty>;
  _field: TRttiProperty;
  f: string;
  jvalue: TJSONValue;
  v: TValue;
  o: TObject;
  list: IWrappedList;
  I: Integer;
  cref: TClass;
  attr: MapperItemsClassType;
  Arr: TJSONArray;
  n: TJSONNumber;
  SerStreamASString: string;
  _attrser: MapperSerializeAsString;
  ListMethod: TRttiMethod;
  ListItem: TValue;
  ListParam: TRttiParameter;
begin
  if not Assigned(AJSONObject) then
    raise EMapperException.Create('JSON Object cannot be nil');
  _type := ctx.GetType(AObject.ClassInfo);
  _fields := _type.GetProperties;
  for _field in _fields do
  begin
    if ((not _field.IsWritable) and (_field.PropertyType.TypeKind <> tkClass))
      or (HasAttribute<MapperTransientAttribute>(_field)) then
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
              raise EMapperException.Create('Invalid value for property ' +
                _field.Name);
          end
          else // it is an enumerated value but it's not a boolean.
          begin
            TValue.Make((jvalue as TJSONNumber).AsInt,
              _field.PropertyType.Handle, v);
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
              _field.SetValue(TObject(AObject),
                ISOStrToDateTime(jvalue.Value + ' 00:00:00'))
          end
          else if _field.PropertyType.QualifiedName = 'System.TDateTime' then
          begin
            if jvalue is TJSONNull then
              _field.SetValue(TObject(AObject), 0)
            else
              _field.SetValue(TObject(AObject), ISOStrToDateTime(jvalue.Value))
          end
          else if _field.PropertyType.QualifiedName = 'System.TTime' then
          begin
            if not(jvalue is TJSONNull) then
              if jvalue is TJSONString then
                _field.SetValue(TObject(AObject), ISOStrToTime(jvalue.Value))
              else
                raise EMapperException.CreateFmt
                  ('Cannot deserialize [%s], expected [%s] got [%s]',
                  [_field.Name, 'TJSONString', jvalue.ClassName]);
          end
          else { if _field.PropertyType.QualifiedName = 'System.Currency' then }
          begin
            if not(jvalue is TJSONNull) then
              if jvalue is TJSONNumber then
                _field.SetValue(TObject(AObject), TJSONNumber(jvalue).AsDouble)
              else
                raise EMapperException.CreateFmt
                  ('Cannot deserialize [%s], expected [%s] got [%s]',
                  [_field.Name, 'TJSONNumber', jvalue.ClassName]);
          end {
            else
            begin
            _field.SetValue(TObject(AObject), (jvalue as TJSONNumber).AsDouble)
            end; }
        end;
      tkString, tkLString, tkWString, tkUString:
        begin
          _field.SetValue(TObject(AObject), jvalue.Value);
        end;
      tkRecord:
        begin
          if _field.PropertyType.QualifiedName = 'System.SysUtils.TTimeStamp'
          then
          begin
            n := jvalue as TJSONNumber;
            _field.SetValue(TObject(AObject),
              TValue.From<TTimeStamp>(MSecsToTimeStamp(n.AsInt64)));
          end;
        end;
      tkClass: // try to restore child properties... but only if the collection is not nil!!!
        begin
          o := _field.GetValue(TObject(AObject)).AsObject;
          if Assigned(o) then
          begin
            if jvalue is TJSONNull then
            begin
              FreeAndNil(o);
              _field.SetValue(AObject, nil);
            end
            else if o is TStream then
            begin
              if jvalue is TJSONString then
              begin
                SerStreamASString := TJSONString(jvalue).Value;
              end
              else
                raise EMapperException.Create('Expected JSONString in ' +
                  AJSONObject.Get(f).JsonString.Value);

              if HasAttribute<MapperSerializeAsString>(_field, _attrser) then
              begin
                DeSerializeStringStream(TStream(o), SerStreamASString,
                  _attrser.Encoding);
              end
              else
              begin
                DeSerializeBase64StringStream(TStream(o), SerStreamASString);
              end;
            end
            else if TDuckTypedList.CanBeWrappedAsList(o) then
            begin // restore collection
              if jvalue is TJSONArray then
              begin
                Arr := TJSONArray(jvalue);
                // look for the MapperItemsClassType on the property itself or on the property type
                if Mapper.HasAttribute<MapperItemsClassType>(_field, attr) or
                  Mapper.HasAttribute<MapperItemsClassType>(_field.PropertyType,
                  attr) then
                begin
                  cref := attr.Value;
                  list := WrapAsList(o);
                  for I := 0 to Arr.Size - 1 do
                  begin
                    list.Add(Mapper.JSONObjectToObject(cref,
                      Arr.Get(I) as TJSONObject));
                  end;
                end
                else // Ezequiel J. Müller convert regular list
                begin
                  ListMethod := ctx.GetType(o.ClassInfo).GetMethod('Add');
                  if (ListMethod <> nil) then
                  begin
                    for I := 0 to Arr.Size - 1 do
                    begin
                      ListItem := TValue.Empty;

                      for ListParam in ListMethod.GetParameters do
                        case ListParam.ParamType.TypeKind of
                          tkInteger, tkInt64:
                            ListItem := StrToIntDef(Arr.Get(I).Value, 0);
                          tkFloat:
                            ListItem := TJSONNumber(Arr.Get(I).Value).AsDouble;
                          tkString, tkLString, tkWString, tkUString:
                            ListItem := Arr.Get(I).Value;
                        end;

                      if not ListItem.IsEmpty then
                        ListMethod.Invoke(o, [ListItem]);
                    end;
                  end;
                end;
              end
              else
                raise EMapperException.Create('Cannot restore ' + f +
                  ' because the related json property is not an array');
            end
            else // try to deserialize into the property... but the json MUST be an object
            begin
              if jvalue is TJSONObject then
              begin
                InternalJSONObjectToObject(ctx, TJSONObject(jvalue), o);
              end
              else if jvalue is TJSONNull then
              begin
                FreeAndNil(o);
                _field.SetValue(AObject, nil);
              end
              else
                raise EMapperException.Create('Cannot deserialize property ' +
                  _field.Name);
            end;
          end;
        end;
    end;
  end;
end;

class function Mapper.JSONObjectToObject(Clazz: TClass;
  AJSONObject: TJSONObject): TObject;
var
  AObject: TObject;
begin
  AObject := TRTTIUtils.CreateObject(Clazz.QualifiedClassName);
  try
    InternalJSONObjectToObject(ctx, AJSONObject, AObject);
    Result := AObject;
  except
    // Ezequiel J. Müller
    // It is important to pass on the exception, to be able to identify the problem you are experiencing.
    on E: Exception do
    begin
      FreeAndNil(AObject);
      raise EMapperException.Create(E.Message);
    end;
  end;
end;

class procedure Mapper.JSONObjectToDataSet(AJSONObject: TJSONObject;
  ADataSet: TDataSet; AJSONObjectInstanceOwner: boolean);
begin
  JSONObjectToDataSet(AJSONObject, ADataSet, TArray<string>.Create(),
    AJSONObjectInstanceOwner);
end;

class procedure Mapper.LoadJSONObjectFieldsStringToObject(AJSONObjectString: string;
  AObject: TObject);
var
  lJSON: TJSONObject;
begin
  lJSON := TJSONObject.ParseJSONValue(AJSONObjectString) as TJSONObject;
  if Assigned(lJSON) then
  begin
    try
      InternalJSONObjectFieldsToObject(ctx, lJSON, AObject);
    finally
      lJSON.Free;
    end;
  end
  else
    EMapperException.Create('Invalid JSON');
end;

class function Mapper.JSONObjectFieldsToObject(AJSONObject
  : TJSONObject): TObject;
var
  lJClassName: TJSONString;
  LObj: TObject;
begin
{$IF CompilerVersion <= 26}
  if Assigned(AJSONObject.Get(DMVC_CLASSNAME)) then
  begin
    lJClassName := AJSONObject.Get(DMVC_CLASSNAME).JsonValue as TJSONString;
  end
  else
    raise EMapperException.Create('No $classname property in the JSON object');
{$ELSE}
  if not AJSONObject.TryGetValue<TJSONString>(DMVC_CLASSNAME, lJClassName) then
    raise EMapperException.Create('No $classname property in the JSON object');
{$ENDIF}
  LObj := TRTTIUtils.CreateObject(lJClassName.Value);
  try
    InternalJSONObjectFieldsToObject(ctx, AJSONObject, LObj);
    Result := LObj;
  except
    FreeAndNil(LObj);
    raise;
  end;
end;

class function Mapper.JSONObjectStringToObject<T>(const AJSONObjectString
  : string): T;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.ParseJSONValue(AJSONObjectString) as TJSONObject;
  if Assigned(JObj) then
  begin
    try
      Result := JSONObjectToObject<T>(JObj);
    finally
      JObj.Free;
    end;
  end
  else
    raise EMapperException.Create('Invalid JSON');
end;

class procedure Mapper.JSONObjectToDataSet(AJSONObject: TJSONObject;
  ADataSet: TDataSet; AIgnoredFields: TArray<string>;
  AJSONObjectInstanceOwner: boolean; AFieldNamePolicy: TFieldNamePolicy);
var
  I: Integer;
  key: string;
  v: TJSONValue;
  jp: TJSONPair;
  fs: TFormatSettings;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    if ContainsFieldName(ADataSet.Fields[I].FieldName, AIgnoredFields) then
      Continue;

    // Name policy { ***** Daniele Spinetti ***** }
    case AFieldNamePolicy of
      fpLowerCase:
        key := LowerCase(ADataSet.Fields[I].FieldName);
      fpUpperCase:
        key := UpperCase(ADataSet.Fields[I].FieldName);
      fpAsIs:
        key := ADataSet.Fields[I].FieldName;
    end;

    v := nil;
    jp := AJSONObject.Get(key);
    if Assigned(jp) then
      if not(jp.JsonValue is TJSONNull) then
        v := AJSONObject.Get(key).JsonValue;
    if not Assigned(v) then
    begin
      ADataSet.Fields[I].Clear;
      Continue;
    end;

    case ADataSet.Fields[I].DataType of
      TFieldType.ftInteger, TFieldType.ftLongWord, TFieldType.ftAutoInc, TFieldType.ftSmallint,
        TFieldType.ftShortint:
        begin
          ADataSet.Fields[I].AsInteger := (v as TJSONNumber).AsInt;
        end;
      TFieldType.ftLargeint:
        begin
          ADataSet.Fields[I].AsLargeInt := (v as TJSONNumber).AsInt64;
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        begin
          ADataSet.Fields[I].AsFloat := (v as TJSONNumber).AsDouble;
        end;
      ftString, ftWideString, ftMemo, ftWideMemo:
        begin
          ADataSet.Fields[I].AsString := (v as TJSONString).Value;
        end;
      TFieldType.ftDate:
        begin
          ADataSet.Fields[I].AsDateTime :=
            ISOStrToDate((v as TJSONString).Value);
        end;
      TFieldType.ftDateTime:
        begin
          ADataSet.Fields[I].AsDateTime :=
            ISOStrToDateTime((v as TJSONString).Value);
        end;
      TFieldType.ftTimeStamp:
        begin
          ADataSet.Fields[I].AsSQLTimeStamp :=
            StrToSQLTimeStamp((v as TJSONString).Value);
        end;
      TFieldType.ftCurrency:
        begin
          fs.DecimalSeparator := '.';
          { ,$IFNDEF TOJSON }
          // ADataSet.Fields[I].AsCurrency :=
          // StrToCurr((v as TJSONString).Value, fs);
          { .$ELSE } // Delphi XE7 introduces method "ToJSON" to fix some old bugs...
          ADataSet.Fields[I].AsCurrency :=
            StrToCurr((v as TJSONNumber).ToJSON, fs);
          { .$IFEND }
        end;
      TFieldType.ftFMTBcd:
        begin
          ADataSet.Fields[I].AsBcd := DoubleToBcd((v as TJSONNumber).AsDouble);
        end;
      TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            SS := TStringStream.Create((v as TJSONString).Value,
              TEncoding.ASCII);
            try
              DecodeStream(SS, MS);
              MS.Position := 0;
              TBlobField(ADataSet.Fields[I]).LoadFromStream(MS);
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;

      TFieldType.ftBoolean:
        begin
{$IFDEF JSONBOOL}
          if v is TJSONBool then
            ADataSet.Fields[I].AsBoolean := (v as TJSONBool).AsBoolean
          else
            raise EMapperException.Create('Invalid JSON for field ' + key);
{$ELSE}
          if v is TJSONTrue then
            ADataSet.Fields[I].AsBoolean := True
          else if v is TJSONFalse then
            ADataSet.Fields[I].AsBoolean := false
          else
            raise EMapperException.Create('Invalid JSON for field ' + key);
{$ENDIF}
        end;
      // else
      // raise EMapperException.Create('Cannot find type for field ' + key);
    end;
  end;
  if AJSONObjectInstanceOwner then
    FreeAndNil(AJSONObject);
end;

class function Mapper.JSONObjectToObject(ClazzName: string;
  AJSONObject: TJSONObject): TObject;
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
      // Result := nil;
      raise; // added 20140630
    end;
  end
  else
    raise EMapperException.CreateFmt('Class not found [%s]', [ClazzName]);
end;

class function Mapper.JSONObjectToObject<T>(AJSONObject: TJSONObject): T;
begin
  if not Assigned(AJSONObject) then
    raise EMapperException.Create('JSONObject not assigned');
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
              _field.SetValue(TObject(AObject),
                (jvalue as TJSONNumber).AsDouble)
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

class procedure Mapper.LoadJSONObjectToObject<T>(AJSONObject: TJSONObject;
  const AObject: T);
begin
  InternalJSONObjectToObject(ctx, AJSONObject, AObject);
end;

class procedure Mapper.DataSetToObjectList<T>(ADataSet: TDataSet;
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
// raise EMapperException.Create('Cannot find type for field ' + key);
// end;
// end;
// if ADataSetInstanceOwner then
// FreeAndNil(ADataSet);
// end;

{$IFDEF USEFIREDAC}


class procedure Mapper.ObjectToFDParameters(AFDParams: TFDParams;
  AObject: TObject; AParamPrefix: string);
var
  I: Integer;
  pname: string;
  _rttiType: TRttiType;
  obj_fields: TArray<TRttiProperty>;
  obj_field: TRttiProperty;
  obj_field_attr: MapperColumnAttribute_DEPRECATED;
  Map: TObjectDictionary<string, TRttiProperty>;
  f: TRttiProperty;
  fv: TValue;
  PrefixLength: Integer;

  function KindToFieldType(AKind: TTypeKind; AProp: TRttiProperty): TFieldType;
  begin
    case AKind of
      tkInteger:
        Result := ftInteger;
      tkFloat:
        begin // daniele teti 2014-05-23
          if AProp.PropertyType.QualifiedName = 'System.TDate' then
            Result := ftDate
          else if AProp.PropertyType.QualifiedName = 'System.TDateTime' then
            Result := ftDateTime
          else if AProp.PropertyType.QualifiedName = 'System.TTime' then
            Result := ftTime
          else
            Result := ftFloat;
        end;
      tkChar, tkString:
        Result := ftString;
      tkWChar, tkUString, tkLString, tkWString:
        Result := ftWideString;
      tkVariant:
        Result := ftVariant;
      tkArray:
        Result := ftArray;
      tkInterface:
        Result := ftInterface;
      tkInt64:
        Result := ftLongWord;
    else
      Result := ftUnknown;
    end;
  end;

begin
  PrefixLength := Length(AParamPrefix);
  Map := TObjectDictionary<string, TRttiProperty>.Create;
  try
    if Assigned(AObject) then
    begin
      _rttiType := ctx.GetType(AObject.ClassType);
      obj_fields := _rttiType.GetProperties;
      for obj_field in obj_fields do
      begin
        if HasAttribute<MapperColumnAttribute_DEPRECATED>(obj_field, obj_field_attr) then
        begin
          Map.Add(MapperColumnAttribute_DEPRECATED(obj_field_attr).FieldName.ToLower,
            obj_field);
        end
        else
        begin
          Map.Add(obj_field.Name.ToLower, obj_field);
        end
      end;
    end;
    for I := 0 to AFDParams.Count - 1 do
    begin
      pname := AFDParams[I].Name.ToLower;
      if pname.StartsWith(AParamPrefix, True) then
        Delete(pname, 1, PrefixLength);
      if Map.TryGetValue(pname, f) then
      begin
        fv := f.GetValue(AObject);
        AFDParams[I].DataType := KindToFieldType(fv.Kind, f);
        // DmitryG - 2014-03-28
        AFDParams[I].Value := fv.AsVariant;
      end
      else
      begin
        AFDParams[I].Clear;
      end;
    end;
  finally
    Map.Free;
  end
end;

class function Mapper.InternalExecuteFDQuery(AQuery: TFDQuery; AObject: TObject;
  WithResult: boolean): Int64;
begin
  ObjectToFDParameters(AQuery.Params, AObject);
  Result := 0;
  if WithResult then
    AQuery.Open
  else
  begin
    AQuery.ExecSQL;
    Result := AQuery.RowsAffected;
  end;
end;

class function Mapper.ExecuteFDQueryNoResult(AQuery: TFDQuery;
  AObject: TObject): Int64;
begin
  Result := InternalExecuteFDQuery(AQuery, AObject, false);
end;

class procedure Mapper.ExecuteFDQuery(AQuery: TFDQuery; AObject: TObject);
begin
  InternalExecuteFDQuery(AQuery, AObject, True);
end;
{$ENDIF}

{$IFDEF USEDBX}


class function Mapper.ExecuteSQLQueryNoResult(AQuery: TSQLQuery;
  AObject: TObject): Int64;
begin
  Result := InternalExecuteSQLQuery(AQuery, AObject, false);
end;

class procedure Mapper.ExecuteSQLQuery(AQuery: TSQLQuery; AObject: TObject);
begin
  InternalExecuteSQLQuery(AQuery, AObject, True);
end;

class function Mapper.ExecuteSQLQueryAsObjectList<T>(AQuery: TSQLQuery;
  AObject: TObject): TObjectList<T>;
begin
  ExecuteSQLQuery(AQuery, AObject);
  Result := TObjectList<T>.Create(True);
  DataSetToObjectList<T>(AQuery, Result);
end;
{$IFEND}

{ MappedField }

constructor MapperColumnAttribute_DEPRECATED.Create(AFieldName: string; AIsPK: boolean);
begin
  inherited Create;
  FFieldName := AFieldName;
  FIsPK := AIsPK;
end;

procedure MapperColumnAttribute_DEPRECATED.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure MapperColumnAttribute_DEPRECATED.SetIsPK(const Value: boolean);
begin
  FIsPK := Value;
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

{ TDataSetHelper }
{ MapperSerializeAsString }

 constructor MapperSerializeAsString.Create(aEncoding: string);
 begin
   inherited Create;
   FEncoding := aEncoding;
 end;

 function MapperSerializeAsString.GetEncoding: string;
 begin
   if FEncoding.IsEmpty then
     FEncoding := DefaultEncoding;
   Result := FEncoding;
 end;

 procedure MapperSerializeAsString.SetEncoding(const Value: string);
 begin
   FEncoding := Value;
 end;

end.
