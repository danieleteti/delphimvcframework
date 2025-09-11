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

unit MVCFramework.UniDAC.Utils;

{$I dmvcframework.inc}

interface

uses
  Uni,
  DBAccess,
  VirtualTable,
  System.Rtti,
  JsonDataObjects,
  Data.DB;

type
  TUniDACUtils = class sealed
  private
    class var CTX: TRttiContext;
    class function InternalExecuteQuery(AQuery: TUniQuery; AObject: TObject;
      WithResult: boolean): Int64;
  public
    class constructor Create;
    class destructor Destroy;
    class function ExecuteQueryNoResult(AQuery: TUniQuery;
      AObject: TObject): Int64;
    class procedure ExecuteQuery(AQuery: TUniQuery; AObject: TObject);
    class procedure ObjectToParameters(AUniParams: TUniParams; AObject: TObject; AParamPrefix: string = '';
      ASetParamTypes: boolean = True);
    class procedure CreateDatasetFromMetadata(AUniMemTable: TCustomUniDataSet; AMeta: TJSONObject);
  end;

  TUniCustomDataSetHelper = class helper for TCustomUniDataSet
  public
    procedure InitFromMetadata(const AJSONMetadata: TJSONObject);
    class function CloneFrom(const ADataSet: TDataSet): TVirtualTable; static;
  end;

implementation

uses
  System.Generics.Collections,
  System.Classes,
  MVCFramework.Serializer.Commons,
  System.SysUtils;

{ TUniDACUtils }

class constructor TUniDACUtils.Create;
begin
  TUniDACUtils.CTX := TRttiContext.Create;
end;

class procedure TUniDACUtils.CreateDatasetFromMetadata(
  AUniMemTable: TCustomUniDataSet; AMeta: TJSONObject);
var
  lJArr: TJSONArray;
  I: Integer;
  lJObj: TJSONObject;
begin
  if AMeta.IsNull('fielddefs') then
  begin
    raise EMVCDeserializationException.Create('Invalid Metadata objects. Property [fielddefs] required.');
  end;

  AUniMemTable.Active := False;;
  AUniMemTable.FieldDefs.Clear;
  lJArr := AMeta.A['fielddefs'];
  for I := 0 to lJArr.Count - 1 do
  begin
    lJObj := lJArr.Items[I].ObjectValue;
    AUniMemTable.FieldDefs.Add(
      lJObj.S['fieldname'],
      TFieldType(lJObj.I['datatype']),
      lJObj.I['size']);
    AUniMemTable.FieldDefs[I].DisplayName := lJObj.S['displayname'];
  end;
  AUniMemTable.Open;
end;

class destructor TUniDACUtils.Destroy;
begin
  TUniDACUtils.CTX.Free;
end;

class procedure TUniDACUtils.ExecuteQuery(AQuery: TUniQuery; AObject: TObject);
begin
  InternalExecuteQuery(AQuery, AObject, True);
end;

class function TUniDACUtils.ExecuteQueryNoResult(AQuery: TUniQuery;
  AObject: TObject): Int64;
begin
  Result := InternalExecuteQuery(AQuery, AObject, False);
end;

class procedure TUniDACUtils.ObjectToParameters(AUniParams: TUniParams;
  AObject: TObject; AParamPrefix: string; ASetParamTypes: boolean);
var
  I: Integer;
  pname: string;
  _rttiType: TRttiType;
  obj_fields: TArray<TRttiProperty>;
  obj_field: TRttiProperty;
  obj_field_attr: MVCColumnAttribute;
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
        Result := ftLargeInt;
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
      _rttiType := CTX.GetType(AObject.ClassType);
      obj_fields := _rttiType.GetProperties;
      for obj_field in obj_fields do
      begin
        if TMVCSerializerHelper.HasAttribute<MVCColumnAttribute>(obj_field, obj_field_attr) then
        begin
          Map.Add(MVCColumnAttribute(obj_field_attr).FieldName.ToLower,
            obj_field);
        end
        else
        begin
          Map.Add(obj_field.Name.ToLower, obj_field);
        end
      end;
    end;
    for I := 0 to AUniParams.Count - 1 do
    begin
      pname := AUniParams[I].Name.ToLower;
      if pname.StartsWith(AParamPrefix, True) then
        Delete(pname, 1, PrefixLength);
      if Map.TryGetValue(pname, f) then
      begin
        fv := f.GetValue(AObject);
        if ASetParamTypes then
        begin
          AUniParams[I].DataType := KindToFieldType(fv.Kind, f);
        end;
        AUniParams[I].Value := fv.AsVariant;
      end
      else
      begin
        AUniParams[I].Clear;
      end;
    end;
  finally
    Map.Free;
  end
end;

class function TUniDACUtils.InternalExecuteQuery(AQuery: TUniQuery; AObject: TObject;
  WithResult: boolean): Int64;
begin
  ObjectToParameters(AQuery.Params, AObject);
  Result := 0;
  if WithResult then
    AQuery.Open
  else
  begin
    AQuery.ExecSQL;
    Result := AQuery.RowsAffected;
  end;
end;

class function TUniCustomDataSetHelper.CloneFrom(const ADataSet: TDataSet): TVirtualTable;
begin
  Result := TVirtualTable.Create(nil);
  Result.Assign(ADataSet);
end;

procedure TUniCustomDataSetHelper.InitFromMetadata(const AJSONMetadata: TJSONObject);
begin
  TUniDACUtils.CreateDatasetFromMetadata(Self, AJSONMetadata);
end;

end.
