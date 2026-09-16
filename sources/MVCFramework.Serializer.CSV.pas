// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
//
// Opt-in CSV serializer for object collections.
// The framework does NOT register this serializer automatically.
// To use it, add it to the Engine in your bootstrap code:
//
//   uses MVCFramework.Serializer.CSV, MVCFramework.Commons;
//   ...
//   FMVCEngine.Serializers.Add(TMVCMediaType.TEXT_CSV, TMVCCSVSerializer.Create);
//
// All parsing and formatting is locale-independent (uses
// TFormatSettings.Invariant as the base).
//
// ***************************************************************************

unit MVCFramework.Serializer.CSV;

{$I dmvcframework.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  System.Generics.Collections,
  Data.SqlTimSt,
  Data.FmtBcd,
  Data.DB,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons;

type
  EMVCCSVSerializerException = class(EMVCSerializationException);

  TMVCCSVSerializerSettings = record
    Delimiter: Char;
    QuoteChar: Char;
    HasHeader: Boolean;
    DateFormat: string;
    DateTimeFormat: string;
    TimeFormat: string;
    DecimalSeparator: Char;
    BooleanTrueValue: string;
    BooleanFalseValue: string;
    TrimValues: Boolean;
    LineEnding: string;
    AlwaysQuoteStrings: Boolean;
    NullValueRepresentation: string;
    class function Default: TMVCCSVSerializerSettings; static;
    class function RFC4180: TMVCCSVSerializerSettings; static;
    class function ExcelEU: TMVCCSVSerializerSettings; static;
  end;

  TMVCCSVSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    FSettings: TMVCCSVSerializerSettings;
    function ParseLines(const ACSV: string): TArray<string>;
    function ParseFields(const ALine: string): TArray<string>;
    procedure SetPropertyFromString(const AObject: TObject;
      const AProp: TRttiProperty; const AValue: string);
    function PropertyValueToString(const AObject: TObject;
      const AProp: TRttiProperty): string;
    function ShouldQuote(const AValue: string): Boolean;
    function EscapeField(const AValue: string): string;
    function IsIgnored(const AProp: TRttiProperty;
      const AIgnoredAttributes: TMVCIgnoredList): Boolean;
  protected
    procedure RaiseNotImplemented;
  public
    constructor Create(const ASettings: TMVCCSVSerializerSettings); reintroduce; overload;
    procedure AfterConstruction; override;
    property Settings: TMVCCSVSerializerSettings read FSettings write FSettings;

    { Low-level row formatting (used by streaming writers) }
    function BuildColumns(const AClazz: TClass;
      const AIgnoredAttributes: TMVCIgnoredList = nil): TArray<TRttiProperty>;
    function BuildHeaderLine(const AColumns: TArray<TRttiProperty>): string;
    function BuildDataLine(const AObject: TObject;
      const AColumns: TArray<TRttiProperty>): string;

    { IMVCSerializer }

    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo;
      AInstance: IMVCTypeSerializer);

    function SerializeObject(
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeObject(
      const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeRecord(
      const ARecord: Pointer;
      const ARecordTypeInfo: PTypeInfo;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeCollection(
      const AList: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeCollection(
      const AList: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeDataSet(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ): string;

    function SerializeDataSetRecord(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ): string;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: string = ''
      ); overload;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil
      ); overload;

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: TObject;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: string = ''
      ); overload;

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: IInterface;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil
      ); overload;

    procedure DeserializeDataSet(
      const ASerializedDataSet: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );

    procedure DeserializeDataSetRecord(
      const ASerializedDataSetRecord: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );

    function SerializeArrayOfRecord(
      var ATValueContainingAnArray: TValue;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil): string;

    procedure DeserializeCollectionOfClass(
      const ACSV: string;
      const AList: TObject;
      const AClazz: TClass;
      const ACustomHeaders: TArray<string>); overload;
  end;

implementation

uses
  System.StrUtils,
  System.DateUtils;

{ TMVCCSVSerializerSettings }

class function TMVCCSVSerializerSettings.Default: TMVCCSVSerializerSettings;
begin
  Result.Delimiter := ',';
  Result.QuoteChar := '"';
  Result.HasHeader := True;
  Result.DateFormat := 'yyyy-mm-dd';
  Result.DateTimeFormat := 'yyyy-mm-dd"T"hh:nn:ss';
  Result.TimeFormat := 'hh:nn:ss';
  Result.DecimalSeparator := '.';
  Result.BooleanTrueValue := 'true';
  Result.BooleanFalseValue := 'false';
  Result.TrimValues := False;
  Result.LineEnding := sLineBreak;
  Result.AlwaysQuoteStrings := False;
  Result.NullValueRepresentation := '';
end;

class function TMVCCSVSerializerSettings.RFC4180: TMVCCSVSerializerSettings;
begin
  Result := Default;
  Result.LineEnding := #13#10;
  Result.AlwaysQuoteStrings := False;
end;

class function TMVCCSVSerializerSettings.ExcelEU: TMVCCSVSerializerSettings;
begin
  Result := Default;
  Result.Delimiter := ';';
  Result.DecimalSeparator := ',';
  Result.LineEnding := #13#10;
end;

{ TMVCCSVSerializer }

procedure TMVCCSVSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
  if FSettings.Delimiter = #0 then
    FSettings := TMVCCSVSerializerSettings.Default;
end;

constructor TMVCCSVSerializer.Create(const ASettings: TMVCCSVSerializerSettings);
begin
  inherited Create;
  FSettings := ASettings;
end;

procedure TMVCCSVSerializer.RaiseNotImplemented;
begin
  raise EMVCCSVSerializerException.Create(
    'Operation not supported by TMVCCSVSerializer');
end;

procedure TMVCCSVSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo;
  AInstance: IMVCTypeSerializer);
begin
  inherited RegisterTypeSerializer(ATypeInfo, AInstance);
end;

function TMVCCSVSerializer.IsIgnored(const AProp: TRttiProperty;
  const AIgnoredAttributes: TMVCIgnoredList): Boolean;
var
  lI: Integer;
begin
  if Length(AIgnoredAttributes) = 0 then
    Exit(False);
  for lI := 0 to High(AIgnoredAttributes) do
    if SameText(AIgnoredAttributes[lI], AProp.Name) then
      Exit(True);
  Result := False;
end;

function TMVCCSVSerializer.ParseLines(const ACSV: string): TArray<string>;
var
  lLines: TList<string>;
  lLen, lI: Integer;
  lInQuoted: Boolean;
  lSb: TStringBuilder;
  lCh: Char;
begin
  lLines := TList<string>.Create;
  try
    lSb := TStringBuilder.Create;
    try
      lInQuoted := False;
      lLen := Length(ACSV);
      lI := 1;
      while lI <= lLen do
      begin
        lCh := ACSV[lI];
        if lInQuoted then
        begin
          if lCh = FSettings.QuoteChar then
          begin
            if (lI < lLen) and (ACSV[lI + 1] = FSettings.QuoteChar) then
            begin
              lSb.Append(lCh);
              lSb.Append(lCh);
              Inc(lI, 2);
            end
            else
            begin
              lInQuoted := False;
              lSb.Append(lCh);
              Inc(lI);
            end;
          end
          else
          begin
            lSb.Append(lCh);
            Inc(lI);
          end;
        end
        else
        begin
          if lCh = FSettings.QuoteChar then
          begin
            lInQuoted := True;
            lSb.Append(lCh);
            Inc(lI);
          end
          else if (lCh = #13) or (lCh = #10) then
          begin
            if lSb.Length > 0 then
              lLines.Add(lSb.ToString);
            lSb.Clear;
            if (lCh = #13) and (lI < lLen) and (ACSV[lI + 1] = #10) then
              Inc(lI, 2)
            else
              Inc(lI);
          end
          else
          begin
            lSb.Append(lCh);
            Inc(lI);
          end;
        end;
      end;
      if lSb.Length > 0 then
        lLines.Add(lSb.ToString);
    finally
      lSb.Free;
    end;
    Result := lLines.ToArray;
  finally
    lLines.Free;
  end;
end;

function TMVCCSVSerializer.ParseFields(const ALine: string): TArray<string>;
var
  lFields: TList<string>;
  lLen, lI: Integer;
  lInQuoted: Boolean;
  lSb: TStringBuilder;
  lCh: Char;
  lFinal: string;
begin
  lFields := TList<string>.Create;
  try
    lSb := TStringBuilder.Create;
    try
      lInQuoted := False;
      lLen := Length(ALine);
      lI := 1;
      while lI <= lLen do
      begin
        lCh := ALine[lI];
        if lInQuoted then
        begin
          if lCh = FSettings.QuoteChar then
          begin
            if (lI < lLen) and (ALine[lI + 1] = FSettings.QuoteChar) then
            begin
              lSb.Append(FSettings.QuoteChar);
              Inc(lI, 2);
            end
            else
            begin
              lInQuoted := False;
              Inc(lI);
            end;
          end
          else
          begin
            lSb.Append(lCh);
            Inc(lI);
          end;
        end
        else
        begin
          if lCh = FSettings.QuoteChar then
          begin
            lInQuoted := True;
            Inc(lI);
          end
          else if lCh = FSettings.Delimiter then
          begin
            lFinal := lSb.ToString;
            if FSettings.TrimValues then
              lFinal := lFinal.Trim;
            lFields.Add(lFinal);
            lSb.Clear;
            Inc(lI);
          end
          else
          begin
            lSb.Append(lCh);
            Inc(lI);
          end;
        end;
      end;
      lFinal := lSb.ToString;
      if FSettings.TrimValues then
        lFinal := lFinal.Trim;
      lFields.Add(lFinal);
    finally
      lSb.Free;
    end;
    Result := lFields.ToArray;
  finally
    lFields.Free;
  end;
end;

procedure TMVCCSVSerializer.SetPropertyFromString(const AObject: TObject;
  const AProp: TRttiProperty; const AValue: string);
var
  lTk: TTypeKind;
  lFmt: TFormatSettings;
  lTmpDate: TDateTime;
  lInt64: Int64;
  lDouble: Double;
  lTypeInfo: PTypeInfo;
begin
  lTypeInfo := AProp.PropertyType.Handle;
  lTk := AProp.PropertyType.TypeKind;

  if AValue = FSettings.NullValueRepresentation then
  begin
    if lTk in [tkUString, tkString, tkLString, tkWString] then
      AProp.SetValue(AObject, '');
    Exit;
  end;

  case lTk of
    tkUString, tkString, tkLString, tkWString:
      AProp.SetValue(AObject, AValue);

    tkChar, tkWChar:
      if Length(AValue) > 0 then
        AProp.SetValue(AObject, AValue[1]);

    tkInteger, tkInt64:
      begin
        lInt64 := StrToInt64(AValue);
        AProp.SetValue(AObject, TValue.FromOrdinal(lTypeInfo, lInt64));
      end;

    tkFloat:
      begin
        if lTypeInfo = TypeInfo(TDateTime) then
        begin
          lFmt := TFormatSettings.Invariant;
          lFmt.DateSeparator := '-';
          lFmt.TimeSeparator := ':';
          lFmt.ShortDateFormat := FSettings.DateTimeFormat;
          if not TryStrToDateTime(AValue, lTmpDate, lFmt) then
          begin
            try
              lTmpDate := ISO8601ToDate(AValue, False);
            except
              raise EMVCCSVSerializerException.CreateFmt(
                'Invalid datetime value "%s" for property %s',
                [AValue, AProp.Name]);
            end;
          end;
          AProp.SetValue(AObject, lTmpDate);
        end
        else if lTypeInfo = TypeInfo(TDate) then
        begin
          lFmt := TFormatSettings.Invariant;
          lFmt.DateSeparator := '-';
          lFmt.ShortDateFormat := FSettings.DateFormat;
          if not TryStrToDate(AValue, lTmpDate, lFmt) then
          begin
            try
              lTmpDate := ISO8601ToDate(AValue, False);
            except
              raise EMVCCSVSerializerException.CreateFmt(
                'Invalid date value "%s" for property %s',
                [AValue, AProp.Name]);
            end;
          end;
          AProp.SetValue(AObject, lTmpDate);
        end
        else if lTypeInfo = TypeInfo(TTime) then
        begin
          lFmt := TFormatSettings.Invariant;
          lFmt.TimeSeparator := ':';
          lFmt.ShortTimeFormat := FSettings.TimeFormat;
          if not TryStrToTime(AValue, lTmpDate, lFmt) then
            raise EMVCCSVSerializerException.CreateFmt(
              'Invalid time value "%s" for property %s',
              [AValue, AProp.Name]);
          AProp.SetValue(AObject, lTmpDate);
        end
        else
        begin
          lFmt := TFormatSettings.Invariant;
          lFmt.DecimalSeparator := FSettings.DecimalSeparator;
          lDouble := StrToFloat(AValue, lFmt);
          case (AProp.PropertyType as TRttiFloatType).FloatType of
            System.TypInfo.ftSingle:   AProp.SetValue(AObject, TValue.From<Single>(lDouble));
            System.TypInfo.ftDouble:   AProp.SetValue(AObject, TValue.From<Double>(lDouble));
            System.TypInfo.ftExtended: AProp.SetValue(AObject, TValue.From<Extended>(lDouble));
            System.TypInfo.ftCurr:     AProp.SetValue(AObject, TValue.From<Currency>(lDouble));
            System.TypInfo.ftComp:     AProp.SetValue(AObject, TValue.From<Comp>(lDouble));
          else
            AProp.SetValue(AObject, lDouble);
          end;
        end;
      end;

    tkEnumeration:
      begin
        if lTypeInfo = TypeInfo(Boolean) then
        begin
          AProp.SetValue(AObject,
            SameText(AValue, FSettings.BooleanTrueValue) or
            SameText(AValue, 'true') or
            (AValue = '1'));
        end
        else
        begin
          lInt64 := GetEnumValue(lTypeInfo, AValue);
          if lInt64 < 0 then
            raise EMVCCSVSerializerException.CreateFmt(
              'Invalid enum value "%s" for property %s',
              [AValue, AProp.Name]);
          AProp.SetValue(AObject, TValue.FromOrdinal(lTypeInfo, lInt64));
        end;
      end;
  else
    raise EMVCCSVSerializerException.CreateFmt(
      'Unsupported type kind for property "%s"', [AProp.Name]);
  end;
end;

procedure TMVCCSVSerializer.DeserializeCollection(const ASerializedList: string;
  const AList: TObject; const AClazz: TClass; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
begin
  DeserializeCollectionOfClass(ASerializedList, AList, AClazz, nil);
end;

procedure TMVCCSVSerializer.DeserializeCollection(const ASerializedList: string;
  const AList: IInterface; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCCSVSerializer.DeserializeCollectionOfClass(const ACSV: string;
  const AList: TObject; const AClazz: TClass;
  const ACustomHeaders: TArray<string>);
var
  lLines: TArray<string>;
  lFields: TArray<string>;
  lHeaders: TArray<string>;
  lWrappedList: IMVCList;
  lRttiType: TRttiType;
  lProps: TArray<TRttiProperty>;
  lI, lJ, lStart: Integer;
  lObj: TObject;
  lProp: TRttiProperty;
begin
  if AList = nil then
    raise EMVCCSVSerializerException.Create('AList is nil');
  if AClazz = nil then
    raise EMVCCSVSerializerException.Create('AClazz is nil');
  if not TDuckTypedList.CanBeWrappedAsList(AList) then
    raise EMVCCSVSerializerException.CreateFmt(
      'AList of type %s cannot be wrapped as list (must expose Add)',
      [AList.ClassName]);

  lWrappedList := WrapAsList(AList, False);
  lRttiType := GetRttiContext.GetType(AClazz);
  if lRttiType = nil then
    raise EMVCCSVSerializerException.CreateFmt(
      'Cannot get RTTI for class %s', [AClazz.ClassName]);

  lLines := ParseLines(ACSV);
  if Length(lLines) = 0 then
    Exit;

  if Length(ACustomHeaders) > 0 then
  begin
    lHeaders := ACustomHeaders;
    if FSettings.HasHeader then
      lStart := 1
    else
      lStart := 0;
  end
  else if FSettings.HasHeader then
  begin
    lHeaders := ParseFields(lLines[0]);
    lStart := 1;
  end
  else
  begin
    raise EMVCCSVSerializerException.Create(
      'CSV without header requires custom headers (Settings.HasHeader=False)');
  end;

  SetLength(lProps, Length(lHeaders));
  for lI := 0 to High(lHeaders) do
    lProps[lI] := lRttiType.GetProperty(lHeaders[lI]);

  for lI := lStart to High(lLines) do
  begin
    lFields := ParseFields(lLines[lI]);
    lObj := AClazz.Create;
    try
      for lJ := 0 to High(lFields) do
      begin
        if lJ > High(lHeaders) then
          Break;
        lProp := lProps[lJ];
        if (lProp = nil) or (not lProp.IsWritable) then
          Continue;
        SetPropertyFromString(lObj, lProp, lFields[lJ]);
      end;
      lWrappedList.Add(lObj);
    except
      lObj.Free;
      raise;
    end;
  end;
end;

function TMVCCSVSerializer.PropertyValueToString(const AObject: TObject;
  const AProp: TRttiProperty): string;
var
  lTk: TTypeKind;
  lTypeInfo: PTypeInfo;
  lValue: TValue;
  lFmt: TFormatSettings;
begin
  lTypeInfo := AProp.PropertyType.Handle;
  lTk := AProp.PropertyType.TypeKind;
  lValue := AProp.GetValue(AObject);

  case lTk of
    tkUString, tkString, tkLString, tkWString:
      Result := lValue.AsString;

    tkChar, tkWChar:
      Result := lValue.AsString;

    tkInteger, tkInt64:
      Result := IntToStr(lValue.AsInt64);

    tkFloat:
      begin
        lFmt := TFormatSettings.Invariant;
        if lTypeInfo = TypeInfo(TDateTime) then
        begin
          lFmt.DateSeparator := '-';
          lFmt.TimeSeparator := ':';
          Result := FormatDateTime(FSettings.DateTimeFormat,
            lValue.AsExtended, lFmt);
        end
        else if lTypeInfo = TypeInfo(TDate) then
        begin
          lFmt.DateSeparator := '-';
          Result := FormatDateTime(FSettings.DateFormat,
            lValue.AsExtended, lFmt);
        end
        else if lTypeInfo = TypeInfo(TTime) then
        begin
          lFmt.TimeSeparator := ':';
          Result := FormatDateTime(FSettings.TimeFormat,
            lValue.AsExtended, lFmt);
        end
        else
        begin
          lFmt.DecimalSeparator := FSettings.DecimalSeparator;
          Result := FloatToStr(lValue.AsExtended, lFmt);
        end;
      end;

    tkEnumeration:
      begin
        if lTypeInfo = TypeInfo(Boolean) then
        begin
          if lValue.AsBoolean then
            Result := FSettings.BooleanTrueValue
          else
            Result := FSettings.BooleanFalseValue;
        end
        else
          Result := GetEnumName(lTypeInfo, lValue.AsOrdinal);
      end;
  else
    Result := '';
  end;
end;

function TMVCCSVSerializer.ShouldQuote(const AValue: string): Boolean;
var
  lI: Integer;
  lCh: Char;
begin
  if FSettings.AlwaysQuoteStrings then
    Exit(True);
  Result := False;
  for lI := 1 to Length(AValue) do
  begin
    lCh := AValue[lI];
    if (lCh = FSettings.Delimiter) or
       (lCh = FSettings.QuoteChar) or
       (lCh = #13) or (lCh = #10) then
      Exit(True);
  end;
end;

function TMVCCSVSerializer.EscapeField(const AValue: string): string;
begin
  if ShouldQuote(AValue) then
    Result := FSettings.QuoteChar +
      StringReplace(AValue,
        FSettings.QuoteChar,
        FSettings.QuoteChar + FSettings.QuoteChar,
        [rfReplaceAll]) +
      FSettings.QuoteChar
  else
    Result := AValue;
end;

function TMVCCSVSerializer.BuildColumns(const AClazz: TClass;
  const AIgnoredAttributes: TMVCIgnoredList): TArray<TRttiProperty>;
var
  lRttiType: TRttiType;
  lAllProps: TArray<TRttiProperty>;
  lProps: TList<TRttiProperty>;
  lI: Integer;
  lProp: TRttiProperty;
begin
  if AClazz = nil then
    raise EMVCCSVSerializerException.Create('AClazz is nil');
  lRttiType := GetRttiContext.GetType(AClazz);
  if lRttiType = nil then
    raise EMVCCSVSerializerException.CreateFmt(
      'Cannot get RTTI for class %s', [AClazz.ClassName]);
  lAllProps := lRttiType.GetProperties;
  lProps := TList<TRttiProperty>.Create;
  try
    for lI := 0 to High(lAllProps) do
    begin
      lProp := lAllProps[lI];
      if not lProp.IsReadable then
        Continue;
      if IsIgnored(lProp, AIgnoredAttributes) then
        Continue;
      lProps.Add(lProp);
    end;
    Result := lProps.ToArray;
  finally
    lProps.Free;
  end;
end;

function TMVCCSVSerializer.BuildHeaderLine(
  const AColumns: TArray<TRttiProperty>): string;
var
  lSb: TStringBuilder;
  lI: Integer;
begin
  if Length(AColumns) = 0 then
    Exit('');
  lSb := TStringBuilder.Create;
  try
    for lI := 0 to High(AColumns) do
    begin
      if lI > 0 then
        lSb.Append(FSettings.Delimiter);
      lSb.Append(EscapeField(AColumns[lI].Name));
    end;
    Result := lSb.ToString;
  finally
    lSb.Free;
  end;
end;

function TMVCCSVSerializer.BuildDataLine(const AObject: TObject;
  const AColumns: TArray<TRttiProperty>): string;
var
  lSb: TStringBuilder;
  lI: Integer;
begin
  if AObject = nil then
    raise EMVCCSVSerializerException.Create('AObject is nil');
  if Length(AColumns) = 0 then
    Exit('');
  lSb := TStringBuilder.Create;
  try
    for lI := 0 to High(AColumns) do
    begin
      if lI > 0 then
        lSb.Append(FSettings.Delimiter);
      lSb.Append(EscapeField(PropertyValueToString(AObject, AColumns[lI])));
    end;
    Result := lSb.ToString;
  finally
    lSb.Free;
  end;
end;

function TMVCCSVSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
var
  lWrappedList: IMVCList;
  lFirst: TObject;
  lRttiType: TRttiType;
  lAllProps: TArray<TRttiProperty>;
  lProps: TList<TRttiProperty>;
  lSb: TStringBuilder;
  lI, lJ: Integer;
  lObj: TObject;
  lProp: TRttiProperty;
begin
  Result := '';
  if AList = nil then
    raise EMVCCSVSerializerException.Create('AList is nil');
  if not TDuckTypedList.CanBeWrappedAsList(AList) then
    raise EMVCCSVSerializerException.CreateFmt(
      'AList of type %s cannot be wrapped as list', [AList.ClassName]);

  lWrappedList := WrapAsList(AList, False);
  if lWrappedList.Count = 0 then
    Exit;

  lFirst := lWrappedList.GetItem(0);
  if lFirst = nil then
    Exit;

  lRttiType := GetRttiContext.GetType(lFirst.ClassType);
  lAllProps := lRttiType.GetProperties;
  lProps := TList<TRttiProperty>.Create;
  try
    for lI := 0 to High(lAllProps) do
    begin
      lProp := lAllProps[lI];
      if not lProp.IsReadable then
        Continue;
      if IsIgnored(lProp, AIgnoredAttributes) then
        Continue;
      lProps.Add(lProp);
    end;

    lSb := TStringBuilder.Create;
    try
      if FSettings.HasHeader then
      begin
        for lI := 0 to lProps.Count - 1 do
        begin
          if lI > 0 then
            lSb.Append(FSettings.Delimiter);
          lSb.Append(EscapeField(lProps[lI].Name));
        end;
        lSb.Append(FSettings.LineEnding);
      end;

      for lI := 0 to lWrappedList.Count - 1 do
      begin
        lObj := lWrappedList.GetItem(lI);
        if lObj = nil then
          Continue;
        for lJ := 0 to lProps.Count - 1 do
        begin
          if lJ > 0 then
            lSb.Append(FSettings.Delimiter);
          lSb.Append(EscapeField(PropertyValueToString(lObj, lProps[lJ])));
        end;
        if lI < lWrappedList.Count - 1 then
          lSb.Append(FSettings.LineEnding);
      end;
      Result := lSb.ToString;
    finally
      lSb.Free;
    end;
  finally
    lProps.Free;
  end;
end;

function TMVCCSVSerializer.SerializeCollection(const AList: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

{ Operations that don't make sense for CSV - all raise EMVCCSVSerializerException }

function TMVCCSVSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCCSVSerializer.SerializeObject(const AObject: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCCSVSerializer.SerializeRecord(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCCSVSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCCSVSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCCSVSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

procedure TMVCCSVSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCCSVSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCCSVSerializer.DeserializeDataSet(const ASerializedDataSet: string;
  const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCCSVSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

end.
