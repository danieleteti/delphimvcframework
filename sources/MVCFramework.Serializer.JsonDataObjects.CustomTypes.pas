// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano M�ller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Serializer.JsonDataObjects.CustomTypes;

{$I dmvcframework.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  JsonDataObjects,
  MVCFramework.Commons, MVCFramework.Serializer.JsonDataObjects;

type

  TMVCStreamSerializerJsonDataObject = class(TInterfacedObject, IMVCTypeSerializer)
  protected
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);

    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);

  public
    { public declarations }
  end;

  TMVCStringDictionarySerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
    // internal use
    class procedure Serialize(const ADict: TMVCStringDictionary; const AJSONObject: TJsonObject); inline;
  end;

  TMVCObjectDictionarySerializer = class(TInterfacedObject, IMVCTypeSerializer)
  protected
    fCurrentSerializer: TMVCJsonDataObjectsSerializer;
  public
    constructor Create(const CurrentSerializer: TMVCJsonDataObjectsSerializer); virtual;
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
  end;

  TMVCDataSetHolderSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
    // internal use
    // class procedure Serialize(const ADict: TMVCStringDictionary; const AJSONObject: TJsonObject); inline;
  end;

  TMVCGUIDSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);

    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
  end;

implementation

uses
  Data.DB,
  MVCFramework.DuckTyping,
  System.Generics.Collections,
  MVCFramework.DataSet.Utils;

procedure TMVCStreamSerializerJsonDataObject.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lStream: TStream;
  SS: TStringStream;
  lJSON: TJDOJsonObject;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  if Assigned(lJSON) then
  begin
    lStream := AElementValue.AsObject as TStream;
    if Assigned(lStream) then
    begin
      lStream.Size := 0;
      SS := TStringStream.Create(lJSON.S[APropertyName]);
      try
        SS.Position := 0;
        if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
        begin
          lStream.CopyFrom(SS, 0);
        end
        else
        begin
          TMVCSerializerHelper.DecodeStream(SS, lStream);
        end;
      finally
        SS.Free;
      end;
    end;
  end;
end;

procedure TMVCStreamSerializerJsonDataObject.DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lValue: TValue;
begin
  lValue := AObject;
  DeserializeAttribute(lValue, 'data', ASerializerObject, AAttributes);
end;

procedure TMVCStreamSerializerJsonDataObject.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lStream: TStream;
  lStringStream: TStringStream;
begin
  lStream := AElementValue.AsObject as TStream;
  if Assigned(lStream) then
  begin
    lStringStream := TStringStream.Create('', TEncoding.Default);
    try
      lStream.Position := 0;
      if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
      begin
        lStringStream.CopyFrom(lStream, 0);
      end
      else
      begin
        TMVCSerializerHelper.EncodeStream(lStream, lStringStream);
      end;
      TJDOJsonObject(ASerializerObject).S[APropertyName] := lStringStream.DataString;
    finally
      lStringStream.Free;
    end;
  end
  else
  begin
    TJsonObject(ASerializerObject).Values[APropertyName] := nil;
  end;
end;

procedure TMVCStreamSerializerJsonDataObject.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);
begin
  ASerializerObject := TJsonObject.Create;
  try
    SerializeAttribute(AObject, 'data', ASerializerObject, AAttributes);
  except
    ASerializerObject.Free;
    raise;
  end;
end;

{ TMVCStringDictionarySerializer }

procedure TMVCStringDictionarySerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lJSON: TJDOJsonObject;
  i: Integer;
begin
  lStringDict := AElementValue.AsObject as TMVCStringDictionary;
  lJSON := ASerializerObject as TJDOJsonObject;
  for i := 0 to lJSON.O[APropertyName].Count - 1 do
  begin
    lStringDict.Add(lJSON.Names[i], lJSON.S[lJSON.Names[i]])
  end;
end;

procedure TMVCStringDictionarySerializer.DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lJSON: TJDOJsonObject;
  i: Integer;
begin
  lStringDict := AObject as TMVCStringDictionary;
  lJSON := ASerializerObject as TJDOJsonObject;
  for i := 0 to lJSON.Count - 1 do
  begin
    lStringDict.Add(lJSON.Names[i], lJSON.S[lJSON.Names[i]])
  end;
end;

class procedure TMVCStringDictionarySerializer.Serialize(const ADict: TMVCStringDictionary;
  const AJSONObject: TJsonObject);
var
  lPair: TPair<string, string>;
begin
  for lPair in ADict do
  begin
    AJSONObject.S[lPair.Key] := lPair.Value;
  end;
end;

procedure TMVCStringDictionarySerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lOutObject: TJsonObject;
  lJsonDict: TJsonObject;
begin
  lStringDict := AElementValue.AsObject as TMVCStringDictionary;
  lOutObject := ASerializerObject as TJsonObject;
  lOutObject.O[APropertyName] := TJsonObject.Create;
  lJsonDict := lOutObject.O[APropertyName];
  if Assigned(lStringDict) then
  begin
    Serialize(lStringDict, lJsonDict);
  end;
end;

procedure TMVCStringDictionarySerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);
var
  lStringDict: TMVCStringDictionary;
  lOutObject: TJsonObject;
begin
  lStringDict := AObject as TMVCStringDictionary;
  lOutObject := TJsonObject.Create;
  if Assigned(lStringDict) then
  begin
    Serialize(lStringDict, lOutObject);
  end;
  ASerializerObject := lOutObject;
end;

{ TMVCGUIDSerializer }

procedure TMVCGUIDSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lJSON: TJDOJsonObject;
  LGuid: TGUID;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  if lJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
    LGuid := TGUID.Empty
  else
    LGuid := TMVCGuidHelper.GuidFromString(lJSON.S[APropertyName]);
  AElementValue := TValue.From<TGUID>(LGuid);
end;

procedure TMVCGUIDSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  // not implemented
end;

procedure TMVCGUIDSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  (ASerializerObject as TJDOJsonObject).S[APropertyName] := AElementValue.AsType<TGUID>.ToString;
end;

procedure TMVCGUIDSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction);
begin
  // not implemented
end;

{ TMVCDataSetHolderSerializer }

procedure TMVCDataSetHolderSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('TDataSetHolder cannot be used as attribute');
end;

procedure TMVCDataSetHolderSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('TDataSetHolder cannot be deserialized');
end;

// class procedure TMVCDataSetHolderSerializer.Serialize(
// const ADict: TMVCStringDictionary; const AJSONObject: TJsonObject);
// begin
//
// end;

procedure TMVCDataSetHolderSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('TDataSetHolder cannot be used as attribute');
end;

procedure TMVCDataSetHolderSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction);
var
  lDataSetHolder: TDataSetHolder;
  lOutObject: TJsonObject;
  lSer: TMVCJsonDataObjectsSerializer;
  lDSFields: TMVCDataSetFields;
begin
  lDataSetHolder := AObject as TDataSetHolder;
  lOutObject := TJsonObject.Create;
  try
    if Assigned(lDataSetHolder) then
    begin
      lSer := TMVCJsonDataObjectsSerializer.Create;
      try
        if lDataSetHolder.SerializationType = TMVCDatasetSerializationType.dstAllRecords then
        begin
          lSer.DataSetToJsonArray(lDataSetHolder.Items, lOutObject.A['data'], TMVCNameCase.ncLowerCase, [])
        end
        else // single record
        begin
          if lDataSetHolder.Items.RecordCount <> 1 then
          begin
            raise EMVCException.CreateFmt('DataSet contains %d records - exactly 1 expected',
              [lDataSetHolder.Items.RecordCount]);
          end;
          lDSFields := lSer.GetDataSetFields(lDataSetHolder.Items, [], TMVCNameCase.ncLowerCase);
          try
            lSer.DataSetToJsonObject(lDataSetHolder.Items, lOutObject.O['data'], TMVCNameCase.ncLowerCase, [],
              lDSFields);
          finally
            lDSFields.Free;
          end;
        end;
        TMVCStringDictionarySerializer.Serialize(lDataSetHolder.Metadata, lOutObject.O['meta']);
      finally
        lSer.Free;
      end;
    end;
  except
    lOutObject.Free;
    raise;
  end;
  ASerializerObject := lOutObject;
end;

{ TMVCObjectDictionarySerializer }

constructor TMVCObjectDictionarySerializer.Create(
  const CurrentSerializer: TMVCJsonDataObjectsSerializer);
begin
  inherited Create;
  fCurrentSerializer := CurrentSerializer;
end;

procedure TMVCObjectDictionarySerializer.DeserializeAttribute(
  var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCDeserializationException.Create('Deserialization not supported for this type');
end;

procedure TMVCObjectDictionarySerializer.DeserializeRoot(
  const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCDeserializationException.Create('Deserialization not supported for this type');
end;

procedure TMVCObjectDictionarySerializer.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCDeserializationException.Create('Serialization as attribute not supported for this type');
end;

procedure TMVCObjectDictionarySerializer.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
var
  lObjDict: TMVCObjectDictionary;
  lOutObject, lOutCustom: TJsonObject;
  lName: string;
  lObj: TMVCObjectDictionary.TMVCObjectDictionaryValueItem;
  lList: IMVCList;
  lLinks: IMVCLinks;

begin
  lObjDict := AObject as TMVCObjectDictionary;
  lOutObject := TJsonObject.Create;
  try
    for lName in lObjDict.Keys do
    begin
      lObj := lObjDict.Items[lName];
      if lObj.Data = nil then
      begin
        lOutObject.O[lName] := nil;
        Continue;
      end;

      if fCurrentSerializer.GetTypeSerializers.ContainsKey(lObj.Data.ClassInfo) then
      begin
        fCurrentSerializer.GetTypeSerializers.Items[lObj.Data.ClassInfo].SerializeRoot(lObj.Data, TObject(lOutCustom),
          [], lObj.SerializationAction);
        lOutObject.O[lName] := lOutCustom;
        Continue;
      end;

      if TDuckTypedList.CanBeWrappedAsList(lObj.Data, lList) then
      begin
        fCurrentSerializer.ListToJsonArray(lList, lOutObject.A[lName], TMVCSerializationType.stDefault, [],
          lObj.SerializationAction)
      end
      else if lObj.Data is TDataSet then
      begin
        case lObj.DataSetSerializationType of
          dstSingleRecord:
            begin
              if TDataSet(lObj.Data).Eof then
              begin
                raise EMVCSerializationException.Create(HTTP_STATUS.InternalServerError,
                  'Cannot serialize a single record of an empty dataset');
              end;
              fCurrentSerializer.InternalSerializeDataSetRecord(
                TDataSet(lObj.Data),
                lOutObject.O[lName],
                [],
                lObj.DataSetFieldNameCase,
                lObj.DataSetSerializationAction)
            end;
          dstAllRecords:
            begin
              fCurrentSerializer.InternalSerializeDataSet(
                TDataSet(lObj.Data),
                lOutObject.A[lName],
                [],
                lObj.DataSetFieldNameCase,
                lObj.DataSetSerializationAction)
            end;
        end;
      end
      else
      begin
        if Assigned(lObj.SerializationAction) then
        begin
          lLinks := TJDOLinks.Create;
        end;
        fCurrentSerializer.InternalObjectToJsonObject(lObj.Data, lOutObject.O[lName],
          TMVCSerializationType.stDefault, [], lObj.SerializationAction, lLinks, nil);

      end;
    end
  except
    lOutObject.Free;
    raise;
  end;
  ASerializerObject := lOutObject;
end;

end.
