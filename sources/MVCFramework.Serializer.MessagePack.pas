// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Serializer.MessagePack;

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
  DMsgPackHelper,
  SimpleMsgPack,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons;

type
  TMVCMessagePackSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  protected
    procedure RaiseNotImplemented;
  public
    procedure AfterConstruction; override;
    { IMVCSerializer }

    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);

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
  end;

implementation

uses
  System.NetEncoding,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DataSet.Utils,
  MVCFramework.Nullables, System.StrUtils;

{ TMVCMessagePackSerializer }

procedure TMVCMessagePackSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TMVCMessagePackSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject; const AClazz: TClass;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.DeserializeDataSet(
  const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCMessagePackSerializer.RaiseNotImplemented;
begin
  raise EMVCException.Create('Not Implemented');
end;

procedure TMVCMessagePackSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo;
  AInstance: IMVCTypeSerializer);
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeCollection(const AList: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
var
  LJObj: TSimpleMsgPack;
  lObjType: TRttiType;
  lDict: IMVCLinks;
begin
  Result := EmptyStr;

  if not Assigned(AObject) then
    Exit('null');

//  if AObject is TSimpleMsgPack then
//    Exit(TSimpleMsgPack(AObject).EncodeToBytes(True));
//
//  if AObject is TDataSet then
//    Exit(self.SerializeDataSet(TDataSet(AObject), AIgnoredAttributes));
//
//  if AObject is System.JSON.TJsonValue then
//    Exit(System.JSON.TJsonValue(AObject).ToJSON);
//
//  lObjType := GetRttiContext.GetType(AObject.ClassType);
//
//  if GetTypeSerializers.ContainsKey(lObjType.Handle) then
//  begin
//    GetTypeSerializers.Items[lObjType.Handle].SerializeRoot(AObject, TObject(LJObj), []);
//    try
//      Result := LJObj.ToJSON(True);
//    finally
//      LJObj.Free;
//    end;
//    Exit;
//  end;
//
//  LJObj := TJDOJsonObject.Create;
//  try
//    if Assigned(ASerializationAction) then
//    begin
//      lDict := TJDOLinks.Create;
//      InternalObjectToJsonObject(AObject, LJObj, GetSerializationType(AObject, AType), AIgnoredAttributes,
//        ASerializationAction, lDict, fStringDictionarySerializer);
//    end
//    else
//    begin
//      InternalObjectToJsonObject(AObject, LJObj, GetSerializationType(AObject, AType), AIgnoredAttributes, nil,
//        nil, nil);
//    end;
//    Result := LJObj.ToJSON(True);
//  finally
//    LJObj.Free;
//  end;
end;

function TMVCMessagePackSerializer.SerializeObject(const AObject: IInterface;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCMessagePackSerializer.SerializeRecord(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  raise Exception.Create('Not implemented');
end;

end.
