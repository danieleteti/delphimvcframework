// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: David Moorhouse (info@moorhouse.net.nz)
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

unit MVCFramework.Serializer.URLEncoded;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.Rtti,
  System.TypInfo, Data.DB,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons,
  System.SysUtils;

type
  TMVCURLEncodedSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    procedure DataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember; const RawData: string;
      const AName: string; var AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>); overload;
  procedure DataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember;
    const RawDataArray: TArray<string>; const AName: string; var AValue: TValue; const AType: TMVCSerializationType;
    const AIgnored: TMVCIgnoredList; const ACustomAttributes: TArray<TCustomAttribute>); overload;

  protected
    procedure RaiseNotImplemented;
  protected
    { IMVCSerializer }
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);

    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeObject(const AObject: IInterface; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeRecord(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil): string; overload;

    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeCollection(const AList: IInterface; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;

    function SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: String = ''); overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: String = ''); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);

    procedure DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);

    function SerializeArrayOfRecord(
      var ATValueContainingAnArray: TValue;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

  public
    procedure URLEncodedStringToObject(
      const Data: TStringList; const AObject: TObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);

  end;

implementation

uses
  System.NetEncoding, System.Math, JsonDataObjects;

{ TMVCURLEncodedSerializer }

procedure TMVCURLEncodedSerializer.DeserializeCollection(const ASerializedList: string; const AList: IInterface;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCURLEncodedSerializer.DeserializeCollection(const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: String);
begin
  RaiseNotImplemented;
end;

procedure TMVCURLEncodedSerializer.DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCURLEncodedSerializer.DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCURLEncodedSerializer.DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  // ??
end;

procedure TMVCURLEncodedSerializer.DeserializeObject(const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: String);
var
  SL: TStringList;
  lPieces: TArray<string>;
  I: Integer;
  lKeyValue: TArray<string>;
begin
  if (ASerializedObject = EmptyStr) then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');

  if not Assigned(AObject) then
    Exit;

  SL := TStringList.Create;
  try
    try
      lPieces := ASerializedObject.Split(['&']);
      for I := 0 to High(lPieces) do
      begin
        lKeyValue := lPieces[I].Split(['=']);
        SL.AddPair(lKeyValue[0], URLDecode(lKeyValue[1]));
      end;
      if GetTypeSerializers.ContainsKey(AObject.ClassInfo) then
      begin
        RaiseNotImplemented;
        // todo: do we handle custom type serialisers
        // GetTypeSerializers.Items[AObject.ClassInfo].DeserializeRoot(SelectRootNodeOrWholeObject(ARootNode, JSONObject),
        // AObject, [])
      end
      else
      begin
        URLEncodedStringToObject(SL, AObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
      end;
    except
      on E: Exception do
        raise EMVCException.Create(HTTP_STATUS.BadRequest, E.Message);
    end;
  finally
    SL.Free;
  end;
end;

procedure TMVCURLEncodedSerializer.RaiseNotImplemented;
begin
  raise EMVCException.Create('Not Implemented');
end;

procedure TMVCURLEncodedSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeCollection(const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeCollection(const AList: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase; const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase; const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeObject(const AObject: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeObject(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCURLEncodedSerializer.SerializeRecord(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

procedure TMVCURLEncodedSerializer.URLEncodedStringToObject(
  const Data: TStringList; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lObjType: TRttiType;
  lProp: TRttiProperty;
  lFld: TRttiField;
  lAttributeValue: TValue;
  lKeyName: string;
  lErrMsg: string;
  I: Integer;
  lArrValues: TArray<String>;
  lCurrIdx: Integer;
  lName: string;
  lTmp: string;
const
  INITIAL_ARRAY_SIZE = 5;
begin
  if AObject = nil then
  begin
    Exit;
  end;

  if TypeInfo(tjsonobject) = AObject.ClassInfo then
  begin
    for I := 0 to Data.Count-1 do
    begin
      lName := Data.Names[I];
      if TJsonObject(AObject).IsNull(lName) then
      begin
        TJsonObject(AObject).S[lName] := Data.ValueFromIndex[I];
      end
      else
      begin
        if TJsonObject(AObject).Types[lName] = jdtString then
        begin
          lTmp := TJsonObject(AObject).S[lName];
          TJsonObject(AObject).Remove(lName);
          TJsonObject(AObject).A[lName].Add(lTmp);
        end;
        TJsonObject(AObject).A[lName].Add(Data.ValueFromIndex[I]);
      end;
    end;
    Exit;
  end;

  lProp := nil;
  lFld := nil;

  lObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        try
          for lProp in lObjType.GetProperties do
          begin
{$IFDEF AUTOREFCOUNT}
            if TMVCSerializerHelper.IsAPropertyToSkip(lProp.Name) then
              continue;
{$ENDIF}
            if ((not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lProp)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, lProp.Name)) and (lProp.IsWritable or lProp.GetValue(AObject).IsObject))
            then
            begin
              lAttributeValue := lProp.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(lProp, lObjType);
              if not lAttributeValue.IsArray then
              begin
                if Data.IndexOfName(lKeyName) > -1 then
                begin
                  DataValueToAttribute(AObject, lProp, Data.Values[lKeyName], lKeyName, lAttributeValue, AType, AIgnoredAttributes,
                    lProp.GetAttributes);
                  if (not lAttributeValue.IsEmpty) and (not lAttributeValue.IsObject) and lProp.IsWritable then
                  begin
                    lProp.SetValue(AObject, lAttributeValue);
                  end;
                end;
              end
              else
              begin
                // there are multiple parameters with the same name
                if Data.IndexOfName(lKeyName) > -1 then
                begin
                  SetLength(lArrValues, INITIAL_ARRAY_SIZE);
                  lCurrIdx := -1;
                  for I := 0 to Data.Count - 1 do
                  begin
                    if Data.Names[i] = lKeyName then
                    begin
                      Inc(lCurrIdx);
                      if (lCurrIdx >= INITIAL_ARRAY_SIZE) and (lCurrIdx >= Length(lArrValues)) then {does short-circuit make it faster?}
                      begin
                        SetLength(lArrValues, Trunc(lCurrIdx * 2));
                      end;
                      lArrValues[lCurrIdx] := Data.ValueFromIndex[i];
                    end;
                  end;
                  SetLength(lArrValues, lCurrIdx + 1); //trim
                  DataValueToAttribute(AObject, lProp, lArrValues, lKeyName,
                    lAttributeValue, AType, AIgnoredAttributes, lProp.GetAttributes);
                  if (not lAttributeValue.IsEmpty) and (not lAttributeValue.IsObject) and lProp.IsWritable then
                  begin
                    lProp.SetValue(AObject, lAttributeValue);
                  end;
                end;
              end;
            end;
          end;
        except
          on E: EInvalidCast do
          begin
            if lProp <> nil then
            begin
              lErrMsg := Format('Invalid class typecast for property "%s" [Expected: %s, Data: %s]',
                [lKeyName, lProp.PropertyType.ToString(), Data.Values[lKeyName]]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for property "%s" [Data: %s]', [lKeyName, Data.Values[lKeyName]]);
            end;
            raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
          end;
        end;
      end;
    stFields:
      begin
        try
          for lFld in lObjType.GetFields do
            if (not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lFld)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, lFld.Name)) then
            begin
              lAttributeValue := lFld.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(lFld, lObjType);
              if Data.IndexOfName(lKeyName) > -1 then
              begin
                DataValueToAttribute(AObject, lFld, Data.Values[lKeyName], lKeyName, lAttributeValue, AType, AIgnoredAttributes,
                  lFld.GetAttributes);
                if (not lAttributeValue.IsEmpty) and (not lAttributeValue.IsObject) then
                  lFld.SetValue(AObject, lAttributeValue);
              end;
            end;
        except
          on E: EInvalidCast do
          begin
            if lFld <> nil then
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Data: %s]',
                [lKeyName, lFld.FieldType.ToString(), Data.Values[lKeyName]]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Data: %s]', [lKeyName, Data.Values[lKeyName]]);
            end;
            raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
          end;
        end;
      end;
  end;
end;

procedure TMVCURLEncodedSerializer.DataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember;
  const RawData: string; const AName: string; var AValue: TValue; const AType: TMVCSerializationType;
  const AIgnored: TMVCIgnoredList; const ACustomAttributes: TArray<TCustomAttribute>);
var
  RttiType: TRttiType;
begin
  RttiType := nil;
  AValue.Empty;
  case AType of
    stUnknown, stDefault, stProperties:
      RttiType := TRttiProperty(ARttiMember).PropertyType;
    stFields:
      RttiType := TRttiField(ARttiMember).FieldType;
  end;

  case RttiType.TypeKind of
    tkString, tkWideString, tkAnsiString, tkUString:
      AValue := TNetEncoding.URL.Decode(RawData);
    tkInteger:
      AValue := RawData.ToInteger;
    tkInt64:
      AValue := RawData.ToInt64;
    tkFloat:
      AValue := RawData.ToDouble;
    tkEnumeration:
      begin
        if SameText(RttiType.ToString, 'boolean') then
          AValue := RawData.ToBoolean;
      end;
    else
      raise EMVCDeserializationException.Create('(DataValueToAttribute) Invalid TypeKind: ' + GetEnumName(TypeInfo(TTypeKind), Ord(RttiType.TypeKind)));
  end;
end;

procedure TMVCURLEncodedSerializer.DataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember;
  const RawDataArray: TArray<string>; const AName: string; var AValue: TValue; const AType: TMVCSerializationType;
  const AIgnored: TMVCIgnoredList; const ACustomAttributes: TArray<TCustomAttribute>);
var
  RttiType: TRttiType;
  //RttiArray: TRttiDynamicArrayType;
begin
  RttiType := nil;
  AValue.Empty;
  case AType of
    stUnknown, stDefault, stProperties:
      RttiType := TRttiProperty(ARttiMember).PropertyType;
    stFields:
      RttiType := TRttiField(ARttiMember).FieldType;
  end;

  if RttiType.TypeKind <> tkDynArray then
  begin
    raise EMVCDeserializationException.Create('Expected DynArray in deserialization for ' + AName);
  end;

  //RttiArray := TRttiDynamicArrayType(RttiType);
  AValue := TValue.From(RawDataArray)
end;


end.

