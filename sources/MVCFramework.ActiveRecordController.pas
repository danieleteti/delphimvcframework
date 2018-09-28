// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.ActiveRecordController;

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.ActiveRecord,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Comp.Client,
  MVCFramework.RQL.Parser;

type
{$SCOPEDENUMS ON}
  TMVCActiveRecordAction = (Create, Retrieve, Update, Delete);
  TMVCActiveRecordAuthFunc = TFunc<TWebContext, TMVCActiveRecordClass, TMVCActiveRecordAction, Boolean>;

  TMVCActiveRecordController = class(TMVCController)
  private
    fAuthorization: TMVCActiveRecordAuthFunc;
    function GetBackEndByConnection(aConnection: TFDConnection): TRQLBackend;
  protected
    function CheckAuthorization(aClass: TMVCActiveRecordClass; aAction: TMVCActiveRecordAction): Boolean; virtual;
  public
    constructor Create(const aConnectionFactory: TFunc<TFDConnection>; const aAuthorization: TMVCActiveRecordAuthFunc = nil); reintroduce;
    destructor Destroy; override;

    [MVCPath('/($entityname)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetEntities(const entityname: string); virtual;

    [MVCPath('/($entityname)/searches')]
    [MVCHTTPMethod([httpGET, httpPOST])]
    procedure GetEntitiesByRQL(const entityname: string); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetEntity(const entityname: string; const id: Integer); virtual;

    [MVCPath('/($entityname)')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateEntity(const entityname: string); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateEntity(const entityname: string; const id: Integer); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteEntity(const entityname: string; const id: Integer); virtual;

  end;

implementation

uses

  MVCFramework.Logger,
  JsonDataObjects;

function TMVCActiveRecordController.GetBackEndByConnection(
  aConnection: TFDConnection): TRQLBackend;
begin
  if aConnection.DriverName = 'FB' then
    Exit(cbFirebird);
  if aConnection.DriverName = 'MySQL' then
    Exit(cbMySQL);
  raise ERQLException.CreateFmt('Unknown driver fro RQL backend "%s"', [aConnection.DriverName]);
end;

procedure TMVCActiveRecordController.GetEntities(const entityname: string);
var
  lARClassRef: TMVCActiveRecordClass;
  lRQL: string;
  lInstance: TMVCActiveRecord;
  lMapping: TMVCFieldsMapping;
  lConnection: TFDConnection;
  lRQLBackend: TRQLBackend;
begin
  lARClassRef := ActiveRecordMappingRegistry.GetByURLSegment(entityname);
  if not CheckAuthorization(lARClassRef, TMVCActiveRecordAction.Retrieve) then
  begin
    Render(TMVCErrorResponse.Create(http_status.Forbidden, 'Cannot read ' + entityname, ''));
    Exit;
  end;
  lRQL := Context.Request.QueryStringParam('rql');
  try
    lConnection := ActiveRecordConnectionsRegistry.GetCurrent;
    lRQLBackend := GetBackEndByConnection(lConnection);
    LogD('[RQL PARSE]: ' + lRQL);
    lInstance := lARClassRef.Create(True);
    try
      lMapping := lInstance.GetMapping;
    finally
      lInstance.Free;
    end;
    Render<TMVCActiveRecord>(TMVCActiveRecord.SelectRQL(lARClassRef, lRQL, lMapping, lRQLBackend), True);
  except
    on E: ERQLCompilerNotFound do
    begin
      LogE('RQL Compiler not found. Did you included MVCFramework.RQL.AST2<yourdatabase>.pas?');
      raise;
    end;
  end;
end;

procedure TMVCActiveRecordController.GetEntitiesByRQL(const entityname: string);
var
  lRQL: string;
  lJSON: TJsonObject;
begin
  if Context.Request.HTTPMethod = httpPOST then
  begin
    lJSON := TJsonObject.Parse(Context.Request.Body) as TJsonObject;
    try
      lRQL := lJSON.s['rql'];
    finally
      lJSON.Free;
    end;
    Context.Request.QueryStringParams.Values['rql'] := lRQL;
  end;
  GetEntities(entityname);
end;

procedure TMVCActiveRecordController.GetEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
begin
  lAR := ActiveRecordMappingRegistry.GetByURLSegment(entityname).Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Retrieve) then
    begin
      Render(TMVCErrorResponse.Create(http_status.Forbidden, 'Cannot read ' + entityname, ''));
      Exit;
    end;

    if lAR.LoadByPK(id) then
    begin
      Render(lAR, False);
    end
    else
    begin
      Render(TMVCErrorResponse.Create(http_status.NotFound, 'Not found', entityname.ToLower + ' not found'));
    end;
  finally
    lAR.Free;
  end;
end;

function TMVCActiveRecordController.CheckAuthorization(aClass: TMVCActiveRecordClass; aAction: TMVCActiveRecordAction): Boolean;
begin
  if Assigned(fAuthorization) then
  begin
    Result := fAuthorization(Context, aClass, aAction);
  end
  else
  begin
    Result := True;
  end;
end;

constructor TMVCActiveRecordController.Create(const aConnectionFactory: TFunc<TFDConnection>;
  const aAuthorization: TMVCActiveRecordAuthFunc = nil);
begin
  inherited Create;
  ActiveRecordConnectionsRegistry.AddConnection('default', aConnectionFactory());
  fAuthorization := aAuthorization;
end;

procedure TMVCActiveRecordController.CreateEntity(const entityname: string);
var
  lAR: TMVCActiveRecord;
begin
  lAR := ActiveRecordMappingRegistry.GetByURLSegment(entityname).Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Create) then
    begin
      Render(TMVCErrorResponse.Create(http_status.Forbidden, 'Cannot create ' + entityname, ''));
      Exit;
    end;

    Context.Request.BodyFor<TMVCActiveRecord>(lAR);
    lAR.Insert;
    StatusCode := http_status.Created;
    Context.Response.CustomHeaders.AddPair('X-REF', Context.Request.PathInfo + '/' + lAR.GetPK.AsInt64.ToString);
    if Context.Request.QueryStringParam('refresh').ToLower = 'true' then
    begin
      Render(lAR, False);
    end;
  finally
    lAR.Free;
  end;
end;

procedure TMVCActiveRecordController.UpdateEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
begin
  lAR := ActiveRecordMappingRegistry.GetByURLSegment(entityname).Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Update) then
    begin
      Render(TMVCErrorResponse.Create(http_status.Forbidden, 'Cannot update ' + entityname, ''));
      Exit;
    end;
    if not lAR.LoadByPK(id) then
      raise EMVCException.Create('Cannot find entity');
    Context.Request.BodyFor<TMVCActiveRecord>(lAR);
    lAR.SetPK(id);
    lAR.Update;
    Context.Response.CustomHeaders.AddPair('X-REF', Context.Request.PathInfo);
    if Context.Request.QueryStringParam('refresh').ToLower = 'true' then
    begin
      Render(lAR, False);
    end
    else
    begin
      Render(http_status.OK, entityname.ToLower + ' updated');
    end;
  finally
    lAR.Free;
  end;
end;

procedure TMVCActiveRecordController.DeleteEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
begin
  lAR := ActiveRecordMappingRegistry.GetByURLSegment(entityname).Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR), TMVCActiveRecordAction.Delete) then
    begin
      Render(TMVCErrorResponse.Create(http_status.Forbidden, 'Cannot delete ' + entityname, ''));
      Exit;
    end;
    if not lAR.LoadByPK(id) then
      raise EMVCException.Create('Cannot find entity');
    Context.Request.BodyFor<TMVCActiveRecord>(lAR);
    lAR.SetPK(id);
    lAR.Delete;
    Render(http_status.OK, entityname.ToLower + ' deleted');
  finally
    lAR.Free;
  end;
end;

destructor TMVCActiveRecordController.Destroy;
begin
  ActiveRecordConnectionsRegistry.RemoveConnection('default');
  inherited;
end;

end.
