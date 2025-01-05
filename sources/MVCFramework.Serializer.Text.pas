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

unit MVCFramework.Serializer.Text;

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
  TMVCTextSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
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

{ TMVCTextSerializer }

procedure TMVCTextSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TMVCTextSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject; const AClazz: TClass;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.DeserializeDataSet(
  const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCTextSerializer.RaiseNotImplemented;
begin
  raise EMVCException.Create('Not Implemented');
end;

procedure TMVCTextSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo;
  AInstance: IMVCTypeSerializer);
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeCollection(const AList: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
  function EmitExceptionClass(const ClazzName, Message: string): string;
  begin
    Result := Result + IfThen(not ClazzName.IsEmpty, ClazzName + ': ') + Message;
  end;
  function EmitTitle(const HTTPStatusCode: Word): string;
  begin
    Result := HTTPStatusCode.ToString + ': ' + HTTP_STATUS.ReasonStringFor(HTTPStatusCode);
  end;

  function GetText(
    const HTTPStatusCode: Integer;
    const Message: String;
    const DetailedMessage: String;
    const ClazzName: String;
    const AppErrorCode: Integer;
    const ErrorItems: TArray<String>): String;
  var
    lErr: String;
  begin
    Result :=
        EmitTitle(HTTPStatusCode) +
        IfThen(not ClazzName.IsEmpty, sLineBreak + EmitExceptionClass(ClazzName, Message)) +
        IfThen(not DetailedMessage.IsEmpty, sLineBreak + DetailedMessage);
    if Assigned(FConfig) then
    begin
      if FConfig[TMVCConfigKey.ExposeServerSignature] = 'true' then
      begin
        Result := Result + sLineBreak + FConfig[TMVCConfigKey.ServerName];
      end;
    end;
    if AppErrorCode <> 0 then
    begin
      Result := sLineBreak + Result + 'Application Error Code: ' + AppErrorCode.ToString + sLineBreak;
    end;
    if Assigned(ErrorItems) and (Length(ErrorItems) > 0) then
    begin
      Result := sLineBreak + Result + 'Error Items: ';
      for lErr in ErrorItems do
      begin
        Result := sLineBreak + Result + '- ' + lErr;
      end;
    end;
  end;
var
  lMVCException: EMVCException;
  lException: Exception;
  lErrResponse: TMVCErrorResponse;
begin
  if AObject is Exception then
  begin
    if AObject is EMVCException then
    begin
      lMVCException := EMVCException(AObject);
      Result := GetText(
        lMVCException.HTTPStatusCode,
        lMVCException.Message,
        lMVCException.DetailedMessage,
        lMVCException.ClassName,
        lMVCException.ApplicationErrorCode,
        lMVCException.ErrorItems);
    end
    else
    begin
      lException := Exception(AObject);
      Result := EmitTitle(500) + sLineBreak +
        EmitExceptionClass(lException.ClassName, lException.Message) + sLineBreak;
    end;
  end else if AObject is TMVCErrorResponse then
  begin
    lErrResponse := TMVCErrorResponse(AObject);
    Result := GetText(
      lErrResponse.StatusCode,
      lErrResponse.Message,
      lErrResponse.DetailedMessage,
      lErrResponse.ClassName,
      lErrResponse.AppErrorCode,
      nil);
  end else if aObject = nil then
  begin
    Result := '';
  end else
  begin
    RaiseNotImplemented;
  end;
end;

function TMVCTextSerializer.SerializeObject(const AObject: IInterface;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCTextSerializer.SerializeRecord(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  raise Exception.Create('Not implemented');
end;

end.
