// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// Contributor of this file: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it
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

unit MVCFramework.Middleware.APIGuard.FireDAC;

{
  APIGuard blacklist in a database table, through a FireDAC connection
  definition. The table must exist:

    CREATE TABLE dmvc_ip_blacklist (
      ip         VARCHAR(64)  NOT NULL PRIMARY KEY,
      rule_name  VARCHAR(64)  NOT NULL,
      reason     VARCHAR(255),
      blocked_at TIMESTAMP    NOT NULL,
      expires_at TIMESTAMP              -- NULL = permanent
    );

  The audit log (TMVCAPIGuardFireDACAuditLog: blocks, unblocks, refused
  requests) goes in another table, one row per event, deleted only by the
  AuditRetentionDays purge:

    CREATE TABLE dmvc_apiguard_audit (
      event_at   TIMESTAMP    NOT NULL,
      event_type VARCHAR(16)  NOT NULL,   -- block, unblock, denied
      ip         VARCHAR(64)  NOT NULL,
      rule_name  VARCHAR(64)  NOT NULL,
      path       VARCHAR(255),
      message    VARCHAR(255)
    );

  The project must also use the FireDAC.Phys.<db> driver unit.
}

{$I dmvcframework.inc}

interface

uses
  MVCFramework.Middleware.APIGuard;

type
  TMVCAPIGuardFireDACStorage = class(TInterfacedObject, IMVCAPIGuardStorage)
  private
    fConnectionDefName: string;
    fTableName: string;
  public
    constructor Create(const AConnectionDefName: string;
      const ATableName: string = 'dmvc_ip_blacklist');
    function Load: TArray<TMVCAPIGuardBlockedIP>;
    procedure Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
  end;

  TMVCAPIGuardFireDACAuditLog = class(TInterfacedObject, IMVCAPIGuardAuditLog)
  private
    fConnectionDefName: string;
    fTableName: string;
  public
    constructor Create(const AConnectionDefName: string;
      const ATableName: string = 'dmvc_apiguard_audit');
    procedure Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);
    procedure Purge(const ABefore: TDateTime);
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DApt,
  MVCFramework.Commons;

function NewConnection(const AConnectionDefName: string): TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  try
    Result.ConnectionDefName := AConnectionDefName;
    Result.LoginPrompt := False;
    Result.Open;
  except
    Result.Free;
    raise;
  end;
end;

function CheckedTableName(const ATableName: string): string;
var
  lChar: Char;
begin
  // the table name is concatenated into the SQL: identifier characters only
  if ATableName = '' then
    raise EMVCException.Create('APIGuard: empty table name');
  for lChar in ATableName do
    if not CharInSet(lChar, ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '.']) then
      raise EMVCException.CreateFmt('APIGuard: invalid table name "%s"', [ATableName]);
  Result := ATableName;
end;

{ TMVCAPIGuardFireDACStorage }

constructor TMVCAPIGuardFireDACStorage.Create(const AConnectionDefName, ATableName: string);
begin
  inherited Create;
  fConnectionDefName := AConnectionDefName;
  fTableName := CheckedTableName(ATableName);
end;

function TMVCAPIGuardFireDACStorage.Load: TArray<TMVCAPIGuardBlockedIP>;
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lList: TList<TMVCAPIGuardBlockedIP>;
  lEntry: TMVCAPIGuardBlockedIP;
begin
  lList := TList<TMVCAPIGuardBlockedIP>.Create;
  try
    lConn := NewConnection(fConnectionDefName);
    try
      lQry := TFDQuery.Create(nil);
      try
        lQry.Connection := lConn;
        lQry.Open('SELECT ip, rule_name, reason, blocked_at, expires_at FROM ' + fTableName);
        while not lQry.Eof do
        begin
          lEntry.IP := lQry.FieldByName('ip').AsString;
          lEntry.Rule := lQry.FieldByName('rule_name').AsString;
          lEntry.Reason := lQry.FieldByName('reason').AsString;
          lEntry.BlockedAt := lQry.FieldByName('blocked_at').AsDateTime;
          if lQry.FieldByName('expires_at').IsNull then
            lEntry.ExpiresAt := 0
          else
            lEntry.ExpiresAt := lQry.FieldByName('expires_at').AsDateTime;
          lList.Add(lEntry);
          lQry.Next;
        end;
      finally
        lQry.Free;
      end;
    finally
      lConn.Free;
    end;
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

procedure TMVCAPIGuardFireDACStorage.Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lEntry: TMVCAPIGuardBlockedIP;
begin
  lConn := NewConnection(fConnectionDefName);
  try
    lQry := TFDQuery.Create(nil);
    try
      lQry.Connection := lConn;
      lConn.StartTransaction;
      try
        lQry.ExecSQL('DELETE FROM ' + fTableName);
        lQry.SQL.Text := 'INSERT INTO ' + fTableName +
          ' (ip, rule_name, reason, blocked_at, expires_at)' +
          ' VALUES (:ip, :rule_name, :reason, :blocked_at, :expires_at)';
        for lEntry in AEntries do
        begin
          lQry.ParamByName('ip').AsString := lEntry.IP;
          lQry.ParamByName('rule_name').AsString := lEntry.Rule;
          lQry.ParamByName('reason').AsString := Copy(lEntry.Reason, 1, 255);
          lQry.ParamByName('blocked_at').AsDateTime := lEntry.BlockedAt;
          lQry.ParamByName('expires_at').DataType := ftDateTime;
          if lEntry.ExpiresAt = 0 then
            lQry.ParamByName('expires_at').Clear
          else
            lQry.ParamByName('expires_at').AsDateTime := lEntry.ExpiresAt;
          lQry.ExecSQL;
        end;
        lConn.Commit;
      except
        lConn.Rollback;
        raise;
      end;
    finally
      lQry.Free;
    end;
  finally
    lConn.Free;
  end;
end;

{ TMVCAPIGuardFireDACAuditLog }

constructor TMVCAPIGuardFireDACAuditLog.Create(const AConnectionDefName, ATableName: string);
begin
  inherited Create;
  fConnectionDefName := AConnectionDefName;
  fTableName := CheckedTableName(ATableName);
end;

procedure TMVCAPIGuardFireDACAuditLog.Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);
var
  lConn: TFDConnection;
  lQry: TFDQuery;
  lEvent: TMVCAPIGuardAuditEvent;
begin
  lConn := NewConnection(fConnectionDefName);
  try
    lQry := TFDQuery.Create(nil);
    try
      lQry.Connection := lConn;
      lQry.SQL.Text := 'INSERT INTO ' + fTableName +
        ' (event_at, event_type, ip, rule_name, path, message)' +
        ' VALUES (:event_at, :event_type, :ip, :rule_name, :path, :message)';
      // one transaction per batch: a flood of "denied" is many rows, one commit
      lConn.StartTransaction;
      try
        for lEvent in AEvents do
        begin
          lQry.ParamByName('event_at').AsDateTime := lEvent.Timestamp;
          lQry.ParamByName('event_type').AsString := Copy(lEvent.Event, 1, 16);
          lQry.ParamByName('ip').AsString := Copy(lEvent.IP, 1, 64);
          lQry.ParamByName('rule_name').AsString := Copy(lEvent.Rule, 1, 64);
          lQry.ParamByName('path').AsString := Copy(lEvent.Path, 1, 255);
          lQry.ParamByName('message').AsString := Copy(lEvent.Message, 1, 255);
          lQry.ExecSQL;
        end;
        lConn.Commit;
      except
        lConn.Rollback;
        raise;
      end;
    finally
      lQry.Free;
    end;
  finally
    lConn.Free;
  end;
end;

procedure TMVCAPIGuardFireDACAuditLog.Purge(const ABefore: TDateTime);
var
  lConn: TFDConnection;
  lQry: TFDQuery;
begin
  lConn := NewConnection(fConnectionDefName);
  try
    lQry := TFDQuery.Create(nil);
    try
      lQry.Connection := lConn;
      lQry.SQL.Text := 'DELETE FROM ' + fTableName + ' WHERE event_at < :before';
      lQry.ParamByName('before').AsDateTime := ABefore;
      lQry.ExecSQL;
    finally
      lQry.Free;
    end;
  finally
    lConn.Free;
  end;
end;

end.
