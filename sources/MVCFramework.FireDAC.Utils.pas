// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.FireDAC.Utils;

{$I dmvcframework.inc}

interface

uses
  FireDAC.Comp.Client, FireDAC.Stan.Param, System.Rtti;

type
  TFireDACUtils = class sealed
  private
    class var CTX: TRttiContext;
    class function InternalExecuteQuery(AQuery: TFDQuery; AObject: TObject;
      WithResult: boolean): Int64;
  public
    class constructor Create;
    class destructor Destroy;
    class function ExecuteQueryNoResult(AQuery: TFDQuery;
      AObject: TObject): Int64;
    class procedure ExecuteQuery(AQuery: TFDQuery; AObject: TObject);
    class procedure ObjectToParameters(AFDParams: TFDParams; AObject: TObject; AParamPrefix: string = ''; ASetParamTypes: Boolean = True);
  end;

implementation

uses
  System.Generics.Collections,
  Data.DB,
  System.Classes,
  MVCFramework.Serializer.Commons,
  System.SysUtils;

{ TFireDACUtils }

class constructor TFireDACUtils.Create;
begin
  TFireDACUtils.CTX := TRttiContext.Create;
end;

class destructor TFireDACUtils.Destroy;
begin
  TFireDACUtils.CTX.Free;
end;

class procedure TFireDACUtils.ExecuteQuery(AQuery: TFDQuery; AObject: TObject);
begin
  InternalExecuteQuery(AQuery, AObject, True);
end;

class function TFireDACUtils.ExecuteQueryNoResult(AQuery: TFDQuery;
  AObject: TObject): Int64;
begin
  Result := InternalExecuteQuery(AQuery, AObject, false);
end;

class procedure TFireDACUtils.ObjectToParameters(AFDParams: TFDParams;
  AObject: TObject; AParamPrefix: string; ASetParamTypes: Boolean);
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
    for I := 0 to AFDParams.Count - 1 do
    begin
      pname := AFDParams[I].Name.ToLower;
      if pname.StartsWith(AParamPrefix, True) then
        Delete(pname, 1, PrefixLength);
      if Map.TryGetValue(pname, f) then
      begin
        fv := f.GetValue(AObject);
        // #001: Erro ao definir parametros
        if ASetParamTypes then
        begin
          AFDParams[I].DataType := KindToFieldType(fv.Kind, f);
        end;
        // #001: FIM
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

class function TFireDACUtils.InternalExecuteQuery(AQuery: TFDQuery; AObject: TObject;
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

end.
