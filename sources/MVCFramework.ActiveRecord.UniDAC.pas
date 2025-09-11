// *************************************************************************** }
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
// ***************************************************************************

unit MVCFramework.ActiveRecord.UniDAC;

{$I dmvcframework.inc}


interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.RTTI,
  Data.DB,
  Uni,
  DBAccess,
  MemDS,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.RQL.Parser,
  MVCFramework.Cache,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  System.SyncObjs,
  System.TypInfo,
  MVCFramework.ActiveRecord;

type
  TMVCActiveRecordUniDAC = class(TMVCActiveRecordBase)
  private
    fConn: TUniConnection;
    function MapNullableTValueToParam(aValue: TValue; const aParam: TParam): Boolean;
  protected
    function GetConnection: TUniConnection;
    procedure MapTValueToParam(aValue: TValue; const aParam: TParam); virtual;
    class function CreateQuery(const Unidirectional, DirectExecute: Boolean): TUniQuery;
    class function ExecQuery(
      const SQL: string;
      const Values: array of Variant;
      const Unidirectional: Boolean;
      const DirectExecute: Boolean): TDataSet; overload;
    class function ExecQuery(
      const SQL: string;
      const Values: array of Variant;
      const Connection: TUniConnection;
      const Unidirectional: Boolean;
      const DirectExecute: Boolean)
      : TDataSet; overload;
    class function ExecQuery(
      const SQL: string;
      const Values: array of Variant;
      const ValueTypes: array of TFieldType;
      const Unidirectional: Boolean;
      const DirectExecute: Boolean)
      : TDataSet; overload;
    class function ExecQuery(
      const SQL: string;
      const Values: array of Variant;
      const ValueTypes: array of TFieldType;
      const Connection: TUniConnection;
      const Unidirectional: Boolean;
      const DirectExecute: Boolean): TDataSet; overload;
    procedure MapObjectToParams(const Params: TParams; var Handled: Boolean); virtual;
  public
    constructor Create(const Connection: TUniConnection); overload;
    class function CurrentConnection: TUniConnection;
    class function GetConnectionByName(const ConnectionName: String): TUniConnection;
  end;

  TMVCActiveRecord = class(TMVCActiveRecordUniDAC)
  end;

implementation

uses
  System.IOUtils,
  System.Classes,
  MVCFramework.DataSet.Utils,
  MVCFramework.Logger,
  MVCFramework.Nullables,
  MVCFramework.RTTI.Utils,
  System.Variants,
  System.Math;

{ TMVCActiveRecordUniDAC }

constructor TMVCActiveRecordUniDAC.Create(const Connection: TUniConnection);
begin
  Create(True);
  fConn := Connection;
end;

class function TMVCActiveRecordUniDAC.CreateQuery(const Unidirectional,
  DirectExecute: Boolean): TUniQuery;
begin
  Result := TUniQuery.Create(nil);
  Result.Unidirectional := Unidirectional;
  Result.Options.DirectExecute := DirectExecute;
end;

class function TMVCActiveRecordUniDAC.CurrentConnection: TUniConnection;
begin
  Result := ActiveRecordConnectionsRegistry.GetCurrent as TUniConnection;
end;

class function TMVCActiveRecordUniDAC.ExecQuery(const SQL: string;
  const Values: array of Variant; const Unidirectional,
  DirectExecute: Boolean): TDataSet;
begin
  Result := ExecQuery(SQL, Values, [], nil, Unidirectional, DirectExecute);
end;

class function TMVCActiveRecordUniDAC.ExecQuery(const SQL: string;
  const Values: array of Variant; const Connection: TUniConnection;
  const Unidirectional, DirectExecute: Boolean): TDataSet;
begin
  Result := ExecQuery(SQL, Values, [], Connection, Unidirectional, DirectExecute);
end;

class function TMVCActiveRecordUniDAC.ExecQuery(const SQL: string;
  const Values: array of Variant; const ValueTypes: array of TFieldType;
  const Unidirectional, DirectExecute: Boolean): TDataSet;
begin
  Result := ExecQuery(SQL, Values, ValueTypes, nil, Unidirectional, DirectExecute);
end;

class function TMVCActiveRecordUniDAC.ExecQuery(const SQL: string;
  const Values: array of Variant; const ValueTypes: array of TFieldType;
  const Connection: TUniConnection; const Unidirectional,
  DirectExecute: Boolean): TDataSet;
var
  lQry: TUniQuery;
  lSQL: string;
  I: Integer;
begin
  lQry := CreateQuery(Unidirectional, DirectExecute);
  try
    lSQL := SQL;
    OnBeforeExecuteQuerySQL(lSQL);

    if Connection = nil then
    begin
      lQry.Connection := ActiveRecordConnectionsRegistry.GetCurrent as TUniConnection;
    end
    else
    begin
      lQry.Connection := Connection;
    end;
    lQry.SQL.Text := lSQL;
    for I := 0 to High(Values) do
    begin
      lQry.Params[I].Value := Values[I];
      if I < Length(ValueTypes) then
      begin
        lQry.Params[I].DataType := ValueTypes[I];
      end;
    end;
    lQry.Open;
    Result := lQry;
  except
    lQry.Free;
    raise;
  end;
end;

function TMVCActiveRecordUniDAC.GetConnection: TUniConnection;
begin
  if fConn = nil then
  begin
    fConn := ActiveRecordConnectionsRegistry.GetCurrent as TUniConnection;
  end;
  Result := fConn;
end;

class function TMVCActiveRecordUniDAC.GetConnectionByName(
  const ConnectionName: String): TUniConnection;
begin
  Result := ActiveRecordConnectionsRegistry.GetByName(ConnectionName) as TUniConnection;
end;

procedure TMVCActiveRecordUniDAC.MapObjectToParams(const Params: TParams;
  var Handled: Boolean);
begin
  // do nothing
end;

function TMVCActiveRecordUniDAC.MapNullableTValueToParam(aValue: TValue;
  const aParam: TParam): Boolean;
var
  lNullableType: TNullableType;
begin
  Assert(aValue.Kind = tkRecord);
  Result := True;
  lNullableType := GetNullableType(aValue.TypeInfo);
  case lNullableType of
    ntInvalidNullableType:
      begin
        Exit(False);
      end;
    ntNullableString:
      begin
        if not aValue.AsType<NullableString>().HasValue then
        begin
          aParam.DataType := ftString;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableString>().Value;
        end;
      end;
    ntNullableCurrency:
      begin
        if not aValue.AsType<NullableCurrency>().HasValue then
        begin
          aParam.DataType := TFieldType.ftCurrency;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableCurrency>().Value;
        end;
      end;
    ntNullableBoolean:
      begin
        if not aValue.AsType<NullableBoolean>().HasValue then
        begin
          aParam.DataType := ftBoolean;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := TValue.From<Boolean>(aValue.AsType<NullableBoolean>().Value);
        end;
      end;
    ntNullableTDate:
      begin
        if not aValue.AsType<NullableTDate>().HasValue then
        begin
          aParam.DataType := ftDate;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := TValue.From<TDate>(aValue.AsType<NullableTDate>().Value);
        end;
      end;
    ntNullableTTime:
      begin
        if not aValue.AsType<NullableTTime>().HasValue then
        begin
          aParam.DataType := ftTime;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := TValue.From<TTime>(aValue.AsType<NullableTTime>().Value);
        end;
      end;
    ntNullableTDateTime:
      begin
        if not aValue.AsType<NullableTDateTime>().HasValue then
        begin
          aParam.DataType := ftDateTime;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := TValue.From<TDateTime>(aValue.AsType<NullableTDateTime>().Value);
        end;
      end;
    ntNullableSingle:
      begin
        if not aValue.AsType<NullableSingle>().HasValue then
        begin
          aParam.DataType := TFieldType.ftSingle;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableSingle>().Value;
        end;
      end;
    ntNullableDouble:
      begin
        if not aValue.AsType<NullableDouble>().HasValue then
        begin
          aParam.DataType := TFieldType.ftFloat;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableDouble>().Value;
        end;
      end;
    ntNullableExtended:
      begin
        if not aValue.AsType<NullableExtended>().HasValue then
        begin
          aParam.DataType := TFieldType.ftExtended;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableExtended>().Value;
        end;
      end;
    ntNullableInt16:
      begin
        if not aValue.AsType<NullableInt16>().HasValue then
        begin
          aParam.DataType := ftInteger;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableInt16>().Value;
        end;
      end;
    ntNullableUInt16:
      begin
        if not aValue.AsType<NullableUInt16>().HasValue then
        begin
          aParam.DataType := ftInteger;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableUInt16>().Value;
        end;
      end;
    ntNullableInt32:
      begin
        if not aValue.AsType<NullableInt32>().HasValue then
        begin
          aParam.DataType := ftInteger;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableInt32>().Value;
        end;
      end;
    ntNullableUInt32:
      begin
        if not aValue.AsType<NullableUInt32>().HasValue then
        begin
          aParam.DataType := ftInteger;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableUInt32>().Value;
        end;
      end;
    ntNullableInt64:
      begin
        if not aValue.AsType<NullableInt64>().HasValue then
        begin
          aParam.DataType := ftLargeInt;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableInt64>().Value;
        end;
      end;
    ntNullableUInt64:
      begin
        if not aValue.AsType<NullableUInt64>().HasValue then
        begin
          aParam.DataType := ftLargeInt;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := aValue.AsType<NullableUInt64>().Value;
        end;
      end;
    ntNullableTGUID:
      begin
        if not aValue.AsType<NullableTGUID>().HasValue then
        begin
          aParam.DataType := TFieldType.ftGuid;
          aParam.Clear;
          Exit(True);
        end
        else
        begin
          aValue := TValue.From<TGuid>(aValue.AsType<NullableTGUID>().Value);
        end;
      end;
  end; // case

  // the nullable value contains a value, so let's call
  // the "non nullable" version of this procedure
  MapTValueToParam(aValue, aParam);
end;

procedure TMVCActiveRecordUniDAC.MapTValueToParam(aValue: TValue;
  const aParam: TParam);
const
  MAX_STRING_PARAM_LENGTH = 1000; { Arbitrary value }
var
  lStream: TStream;
  lName: string;
begin
{$IFDEF NEXTGEN}
  lName := aValue.TypeInfo.NameFld.ToString;
{$ELSE}
  lName := string(aValue.TypeInfo.Name);
{$ENDIF}
  if (lName.StartsWith('Nullable', True) and (aValue.TypeInfo.Kind = tkRecord)) then
  begin
    if MapNullableTValueToParam(aValue, aParam) then
    begin
      Exit;
    end;
  end;

  case aValue.TypeInfo.Kind of
    tkUString:
      begin
        case aParam.DataType of
          ftUnknown, ftWideString:
            begin
              if aValue.AsString.Length > MAX_STRING_PARAM_LENGTH then
              begin
                aParam.AsWideMemo := aValue.AsString;
              end
              else
              begin
                aParam.AsWideString := aValue.AsString;
              end;
            end;
          ftString:
            begin
              if aValue.AsString.Length > MAX_STRING_PARAM_LENGTH then
              begin
                aParam.AsMemo := AnsiString(aValue.AsString);
              end
              else
              begin
                aParam.AsString := aValue.AsString;
              end;
            end;
          ftWideMemo:
            begin
              aParam.AsWideMemo := aValue.AsString;
            end;
          ftMemo:
            begin
              aParam.AsMemo := AnsiString(aValue.AsString);
            end;
        else
          begin
            raise EMVCActiveRecord.CreateFmt('Invalid parameter type for (tkUString) [%s]', [lName]);
          end;
        end;
      end;
    tkString:
      begin
        case aParam.DataType of
          ftUnknown, ftWideString:
            begin
              if aValue.AsString.Length > MAX_STRING_PARAM_LENGTH then
              begin
                aParam.AsWideMemo := aValue.AsString;
              end
              else
              begin
                aParam.AsWideString := aValue.AsString;
              end;
            end;
          ftString:
            begin
              if aValue.AsString.Length > MAX_STRING_PARAM_LENGTH then
              begin
                aParam.AsMemo := AnsiString(aValue.AsString);
              end
              else
              begin
                aParam.AsString := aValue.AsString;
              end;
            end;
          ftWideMemo:
            begin
              aParam.AsWideMemo := aValue.AsString;
            end;
          ftMemo:
            begin
              aParam.AsMemo := AnsiString(aValue.AsString);
            end;
        else
          begin
            raise EMVCActiveRecord.CreateFmt('Invalid parameter type for (tkString) [%s]', [lName]);
          end;
        end;
      end;
{$IF Defined(SeattleOrBetter)}
    tkWideString:
      begin
        if aValue.AsString.Length > MAX_STRING_PARAM_LENGTH then
        begin
          aParam.AsWideMemo := aValue.AsString;
        end
        else
        begin
          aParam.AsWideString := aValue.AsString;
        end
      end;
{$ENDIF}
    tkInt64:
      begin
        aParam.AsLargeInt := aValue.AsInt64;
      end;
    tkInteger:
      begin
        aParam.AsInteger := aValue.AsInteger;
      end;
    tkEnumeration:
      begin
        if aValue.TypeInfo = TypeInfo(System.Boolean) then
        begin
          if aParam.DataTypeName.StartsWith('int', true) then
          begin
            aParam.AsInteger := IfThen(aValue.AsBoolean,1,0);
          end
          else
          begin
            aParam.AsBoolean := aValue.AsBoolean;
          end;
        end
        else
        begin
          aParam.AsInteger := aValue.AsOrdinal;
        end;
      end;
    tkFloat:
      begin
        if lName = 'TDate' then
        begin
          aParam.AsDate := Trunc(aValue.AsExtended);
        end
        else if lName = 'TDateTime' then
        begin
          aParam.AsDateTime := aValue.AsExtended;
        end
        else if lName = 'TTime' then
        begin
          aParam.AsTime := aValue.AsExtended;
        end
        else if lName = 'Currency' then
        begin
          aParam.AsCurrency := aValue.AsCurrency;
        end
        else
        begin
          aParam.AsFloat := aValue.AsExtended;
        end;
      end;
    tkClass:
      begin
        if (aValue.AsObject <> nil) and (not aValue.IsInstanceOf(TStream)) then
          raise EMVCActiveRecord.CreateFmt('Unsupported reference type for param %s: %s',
            [aParam.Name, aValue.AsObject.ClassName]);
        lStream := aValue.AsType<TStream>();
        if Assigned(lStream) then
        begin
          lStream.Position := 0;
          aParam.LoadFromStream(lStream, ftBlob);
        end
        else
        begin
          aParam.DataType := TFieldType.ftBlob;
          aParam.Clear;
        end;
      end;
    tkRecord:
      begin
        if aValue.TypeInfo = TypeInfo(TGuid) then
        begin
          if SQLGenerator.HasNativeUUID then
          begin
            aParam.AsGuid := aValue.AsType<TGuid>
          end
          else
          begin
            aParam.AsString := GUIDToString(aValue.AsType<TGuid>);
          end;
        end
        else if aValue.TypeInfo = TypeInfo(NullableTGUID) then
        begin
          if aValue.AsType<NullableTGUID>.HasValue then
            aParam.AsGuid := aValue.AsType<NullableTGUID>.Value
          else
            aParam.Clear();
        end
        else
        begin
          raise EMVCActiveRecord.CreateFmt('Unsupported Record TypeKind (%d) for param %s',
            [Ord(aValue.TypeInfo.Kind), aParam.Name]);
        end;
      end;
  else
    raise EMVCActiveRecord.CreateFmt('Unsupported TypeKind (%d) for param %s', [Ord(aValue.TypeInfo.Kind), aParam.Name]);
  end;
end;

end.
