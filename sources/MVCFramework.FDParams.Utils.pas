// ***************************************************************************
//
// TNullabletypes requires spring4d framework
//
// https://bitbucket.org/sglienke/spring4d.git
//
// Contributor: 2019 - João Antônio Duarte (joao.antonioduarte@hotmail.com)
//
// ***************************************************************************
unit MVCFramework.FDParams.Utils;

interface

uses
  System.Rtti,
  FireDAC.Stan.Param;

type
  TFDParamsUtils = class sealed
  private
    class var FCtx: TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure ObjectToParams(AFDParams: TFDParams; AObject: TObject; AParamPrefix: string = '');
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  Data.DB, MVCFramework.NullableTypes;

{ TFDParamsUtils }

class constructor TFDParamsUtils.Create;
begin
  TFDParamsUtils.FCtx := TRttiContext.Create;
end;

class destructor TFDParamsUtils.Destroy;
begin
  TFDParamsUtils.FCtx.Free;
end;

class procedure TFDParamsUtils.ObjectToParams(AFDParams: TFDParams; AObject: TObject; AParamPrefix: string);
var
  I: Integer;
  LPName: string;
  LRttiType: TRttiType;
  LObjFields: TArray<TRttiProperty>;
  LObjField: TRttiProperty;
  LObjFieldAttr: MVCColumnAttribute;
  LMap: TObjectDictionary<string, TRttiProperty>;
  LRttiProp: TRttiProperty;
  LValue: TValue;
  LPrefixLength: Integer;

  function KindToFieldType(AKind: TTypeKind; AProp: TRttiProperty): TFieldType;
  begin
    case AKind of
      tkInteger:
        Result := ftInteger;
      tkFloat:
        begin
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
  LPrefixLength := Length(AParamPrefix);
  LMap := TObjectDictionary<string, TRttiProperty>.Create;
  try
    if Assigned(AObject) then
    begin
      LRttiType := FCtx.GetType(AObject.ClassType);
      LObjFields := LRttiType.GetProperties;
      for LObjField in LObjFields do
      begin
        if TMVCSerializerHelper.HasAttribute<MVCColumnAttribute>(LObjField, LObjFieldAttr) then
        begin
          LMap.Add(MVCColumnAttribute(LObjFieldAttr).FieldName.ToLower, LObjField);
        end
        else
        begin
          LMap.Add(LObjField.Name.ToLower, LObjField);
        end
      end;
    end;
    for I := 0 to AFDParams.Count - 1 do
    begin
      LPName := AFDParams[I].Name.ToLower;
      if LPName.StartsWith(AParamPrefix, True) then
        Delete(LPName, 1, LPrefixLength);
      if LMap.TryGetValue(LPName, LRttiProp) then
      begin
        LValue := LRttiProp.GetValue(AObject);

        if LValue.TypeInfo = TypeInfo(TNullableInteger) then
        begin
          AFDParams[I].DataType := ftInteger;
          AFDParams[I].Value := LValue.AsType<TNullableInteger>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableInt64) then
        begin
          AFDParams[I].DataType := ftLargeint;
          AFDParams[I].Value := LValue.AsType<TNullableInt64>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableCurrency) then
        begin
          AFDParams[I].DataType := ftCurrency;
          AFDParams[I].Value := LValue.AsType<TNullableCurrency>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableString) then
        begin
          AFDParams[I].DataType := ftWideString;
          AFDParams[I].Value := LValue.AsType<TNullableString>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableDateTime) then
        begin
          AFDParams[I].DataType := ftDateTime;
          AFDParams[I].Value := LValue.AsType<TNullableDateTime>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableDate) then
        begin
          AFDParams[I].DataType := ftDate;
          AFDParams[I].Value := LValue.AsType<TNullableDate>.ToVariant;
        end
        else if LValue.TypeInfo = TypeInfo(TNullableTime) then
        begin
          AFDParams[I].DataType := ftTime;
          AFDParams[I].Value := LValue.AsType<TNullableTime>.ToVariant;
        end
        else if (LValue.TypeInfo = TypeInfo(TStream)) or
          (LValue.TypeInfo = TypeInfo(TMemoryStream)) or
          (LValue.TypeInfo = TypeInfo(TStringStream)) then
        begin
          AFDParams[I].DataType := ftBlob;
          if TStream(LValue.AsObject).Size > 0 then
            AFDParams[I].LoadFromStream(TStream(LValue.AsObject), ftBlob)
          else
            AFDParams[I].Clear;
        end
        else
        begin
          AFDParams[I].DataType := KindToFieldType(LValue.Kind, LRttiProp);
          AFDParams[I].Value := LValue.AsVariant;
        end;
      end
      else
      begin
        AFDParams[I].Clear;
      end;
    end;
  finally
    LMap.Free;
  end
end;

end.
