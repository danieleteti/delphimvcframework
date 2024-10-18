// *************************************************************************** }
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
  MVCFramework.RQL.Parser,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons, MVCFramework.Swagger.Commons;

type
{$SCOPEDENUMS ON}
  TMVCActiveRecordAction = (Create, Retrieve, Update, Delete);
  TMVCActiveRecordAuthFunc = TFunc<TWebContext, TMVCActiveRecordClass, TMVCActiveRecordAction, Boolean>;

  TMVCActiveRecordController = class(TMVCController)
  private
    fAuthorization: TMVCActiveRecordAuthFunc;
    fURLSegment: string;
  protected
    function GetMaxRecordCount: Integer;
    function CheckAuthorization(aClass: TMVCActiveRecordClass; aAction: TMVCActiveRecordAction): Boolean; virtual;
  public
    constructor Create(
      const aAuthorization: TMVCActiveRecordAuthFunc = nil;
      const aURLSegment: String = ''); reintroduce; overload;
    destructor Destroy; override;

    function GetURLSegment: String;

    [MVCPath('/($entityname)')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Retrieve a list of {singularmodel}', 'Get{pluralmodel}')]
    [MVCSwagResponses(HTTP_STATUS.OK, 'List of {singularmodel}', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    [MVCSwagParam(TMVCSwagParamLocation.plQuery, 'rql', 'RQL filter used to filter the list of {singularmodel}', SWAGUseDefaultControllerModel, TMVCSwagParamType.ptString, False)]
    procedure GetEntities(const entityname: string); virtual;

    [MVCPath('/($entityname)/searches')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Searches through {pluralmodel} and returns a list of {singularmodel}', 'Get{pluralmodel}BySearch')]
    [MVCSwagResponses(HTTP_STATUS.OK, 'List of {singularmodel}', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    [MVCSwagParam(TMVCSwagParamLocation.plQuery, 'rql', 'RQL filter used to filter the list of {singularmodel}', SWAGUseDefaultControllerModel, TMVCSwagParamType.ptString, False)]
    procedure GetEntitiesByRQL(const entityname: string); virtual;

    [MVCPath('/($entityname)/searches')]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Searches through {pluralmodel} and returns a list of {singularmodel}', 'Get{pluralmodel}BySearchAsPOST')]
    [MVCSwagResponses(HTTP_STATUS.OK, 'List of {singularmodel}', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    [MVCSwagParam(TMVCSwagParamLocation.plQuery, 'rql', 'RQL filter used to filter the list of {singularmodel}', SWAGUseDefaultControllerModel, TMVCSwagParamType.ptString, False)]
    procedure GetEntitiesByRQLwithPOST(const entityname: string); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Gets a {singularmodel} entity or 404 not found', 'Get{singularmodel}ByID')]
    [MVCSwagResponses(HTTP_STATUS.OK, 'One {singularmodel}', SWAGUseDefaultControllerModel)]
    [MVCSwagResponses(HTTP_STATUS.NotFound, 'Error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    procedure GetEntity(const entityname: string; const id: Integer); virtual;

    [MVCPath('/($entityname)')]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Creates a {singularmodel} and returns a new id', 'Create{singularmodel}')]
    [MVCSwagResponses(HTTP_STATUS.Created, 'One {singularmodel}', '')]
    [MVCSwagResponses(HTTP_STATUS.NotFound, 'Error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    [MVCSwagParam(TMVCSwagParamLocation.plBody, '{singularmodel}', 'A single entity of type {singularmodel}', SWAGUseDefaultControllerModel, TMVCSwagParamType.ptString, True)]
    procedure CreateEntity(const entityname: string); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpPUT])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Updates a {singularmodel} by id', 'Update{singularmodel}ByID')]
    [MVCSwagResponses(HTTP_STATUS.OK, 'One {singularmodel}', SWAGUseDefaultControllerModel)]
    [MVCSwagResponses(HTTP_STATUS.NotFound, 'Error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    [MVCSwagParam(TMVCSwagParamLocation.plBody, '{singularmodel}', 'A single entity of type {singularmodel}', SWAGUseDefaultControllerModel, TMVCSwagParamType.ptString, True)]
    procedure UpdateEntity(const entityname: string; const id: Integer); virtual;

    [MVCPath('/($entityname)/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS, 'Deletes a {singularmodel} by id', 'Delete{singularmodel}ByID')]
    [MVCSwagResponses(HTTP_STATUS.NoContent, '')]
    [MVCSwagResponses(HTTP_STATUS.NotFound, 'Error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
    procedure DeleteEntity(const entityname: string; const id: Integer); virtual;

  end;

  [MVCNameCase(ncLowerCase)]
  TMVCActiveRecordListResponse = class
  private
    FList: TMVCActiveRecordList;
    FMetadata: TMVCStringDictionary;
    FOwns: Boolean;
  public
    constructor Create(AList: TMVCActiveRecordList; AOwns: Boolean = True); virtual;
    destructor Destroy; override;
    [MVCListOf(TMVCActiveRecord)]
    [MVCNameAs('data')]
    property Items: TMVCActiveRecordList read FList;
    [MVCNameAs('meta')]
    property Metadata: TMVCStringDictionary read FMetadata;
  end;

implementation

uses
  MVCFramework.Logger,
  JsonDataObjects,
  Data.DB;

procedure TMVCActiveRecordController.GetEntities(const entityname: string);
var
  lARClassRef: TMVCActiveRecordClass;
  lRQL: string;
  lInstance: TMVCActiveRecord;
  lMapping: TMVCFieldsMapping;
  lProcessor: IMVCEntityProcessor;
  lHandled: Boolean;
  lARResp: TMVCActiveRecordList;
  lStrDict : TMVCStringDictionary;
begin
  lProcessor := nil;
  if ActiveRecordMappingRegistry.FindProcessorByURLSegment(entityname, lProcessor) then
  begin
    lHandled := False;
    lProcessor.GetEntities(Context, self, entityname, lHandled);
    if lHandled then
    begin
      Exit;
    end;
  end;

  if not ActiveRecordMappingRegistry.FindEntityClassByURLSegment(entityname, lARClassRef) then
  begin
    raise EMVCException.CreateFmt('Cannot find entity nor processor for entity "%s"', [entityname]);
  end;
  if not CheckAuthorization(lARClassRef, TMVCActiveRecordAction.Retrieve) then
  begin
    Render(TMVCErrorResponse.Create(HTTP_STATUS.Forbidden, 'Cannot read ' + entityname));
    Exit;
  end;

  lRQL := Context.Request.QueryStringParam('rql');
  try
    LogD('[RQL PARSE]: ' + lRQL);
    lInstance := lARClassRef.Create(True);
    try
      lMapping := lInstance.GetMapping;
    finally
      lInstance.Free;
    end;

    lARResp := TMVCActiveRecord.SelectRQL(lARClassRef, lRQL, GetMaxRecordCount);
    try
      lStrDict := StrDict(['page_size'],[lARResp.Count.ToString]);
      try
        if Context.Request.QueryStringParam('count').ToLower = 'true' then
        begin
          lStrDict.Add('count', TMVCActiveRecord.Count(lARClassRef, lRQL).ToString);
        end;
        Render(ObjectDict(False)
          .Add('data', lARResp,
            procedure(const AObject: TObject; const Links: IMVCLinks)
            begin
              //Links.AddRefLink.Add(HATEOAS.HREF, fURLSegment + '/' + )
              case TMVCActiveRecord(AObject).GetPrimaryKeyFieldType of
                ftInteger:
                  Links.AddRefLink.Add(HATEOAS.HREF, fURLSegment + '/' + TMVCActiveRecord(AObject).GetPK.AsInt64.ToString)
              end;
            end)
          .Add('meta', lStrDict));
      finally
        lStrDict.Free;
      end;
    finally
      lARResp.Free;
    end;

  except
    on E: ERQLCompilerNotFound do
    begin
      LogE('RQL Compiler not found. Did you included MVCFramework.RQL.AST2<yourdatabase>.pas?');
      raise;
    end;
  end;
end;

procedure TMVCActiveRecordController.GetEntitiesByRQL(const entityname: string);
begin
  GetEntities(entityname);
end;

procedure TMVCActiveRecordController.GetEntitiesByRQLwithPOST(const entityname: string);
var
  lRQL: string;
  lJSON: TJsonObject;
begin
  lJSON := TJsonObject.Parse(Context.Request.Body) as TJsonObject;
  try
    if Assigned(lJSON) then
    begin
      lRQL := lJSON.s['rql'];
    end
    else
    begin
      lRQL := '';
    end;
  finally
    lJSON.Free;
  end;
  Context.Request.QueryStringParams.Values['rql'] := lRQL;
  GetEntities(entityname);
end;


procedure TMVCActiveRecordController.GetEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
  lARClass: TMVCActiveRecordClass;
  lProcessor: IMVCEntityProcessor;
  lHandled: Boolean;
  lResponse: IMVCResponse;
begin
  lProcessor := nil;
  if ActiveRecordMappingRegistry.FindProcessorByURLSegment(entityname, lProcessor) then
  begin
    lHandled := False;
    lProcessor.GetEntity(Context, self, entityname, id, lHandled);
    if lHandled then
    begin
      Exit;
    end;
  end;

  if not ActiveRecordMappingRegistry.FindEntityClassByURLSegment(entityname, lARClass) then
  begin
    raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Cannot find entity %s', [entityname]);
  end;
  lAR := lARClass.Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Retrieve) then
    begin
      Render(TMVCErrorResponse.Create(HTTP_STATUS.Forbidden, 'Cannot read ' + entityname));
      Exit;
    end;

    if lAR.LoadByPK(id) then
    begin
      lResponse := MVCResponseBuilder
          .StatusCode(HTTP_STATUS.OK)
          .Body(ObjectDict(false).Add('data', lAR))
          .Build;
    end
    else
    begin
      lResponse := MVCResponseBuilder
          .StatusCode(HTTP_STATUS.NotFound)
          .Body(entityname.ToLower + ' not found')
          .Build;
    end;
    TMVCRenderer.InternalRenderMVCResponse(Self, TMVCResponse(lResponse));
  finally
    lAR.Free;
  end;
end;

function TMVCActiveRecordController.GetMaxRecordCount: Integer;
begin
  Result := StrToIntDef(Config[TMVCConfigKey.MaxEntitiesRecordCount], 20);
end;

function TMVCActiveRecordController.GetURLSegment: String;
begin
  Result := fURLSegment;
end;

function TMVCActiveRecordController.CheckAuthorization(aClass: TMVCActiveRecordClass;
  aAction: TMVCActiveRecordAction): Boolean;
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

constructor TMVCActiveRecordController.Create(
  const aAuthorization: TMVCActiveRecordAuthFunc;
  const aURLSegment: String);
begin
  inherited Create;
  fURLSegment := aURLSegment;
  fAuthorization := aAuthorization;
end;

procedure TMVCActiveRecordController.CreateEntity(const entityname: string);
var
  lAR: TMVCActiveRecord;
  lARClass: TMVCActiveRecordClass;
  lProcessor: IMVCEntityProcessor;
  lHandled: Boolean;
begin
  lProcessor := nil;
  if ActiveRecordMappingRegistry.FindProcessorByURLSegment(entityname, lProcessor) then
  begin
    lHandled := False;
    lProcessor.CreateEntity(Context, self, entityname, lHandled);
    if lHandled then
    begin
      Exit;
    end;
  end;

  if not ActiveRecordMappingRegistry.FindEntityClassByURLSegment(entityname, lARClass) then
  begin
    raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Cannot find entity %s', [entityname]);
  end;
  lAR := lARClass.Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Create) then
    begin
      Render(TMVCErrorResponse.Create(HTTP_STATUS.Forbidden, 'Cannot create ' + entityname));
      Exit;
    end;

    Context.Request.BodyFor<TMVCActiveRecord>(lAR);
    lAR.Insert;
    Context.Response.CustomHeaders.Values['X-REF'] := Context.Request.PathInfo + '/' + lAR.GetPK.AsInt64.ToString;
    if Context.Request.QueryStringParam('refresh').ToLower = 'true' then
    begin
      RenderStatusMessage(HTTP_STATUS.Created, entityname.ToLower + ' created', '', lAR, False);
    end
    else
    begin
      RenderStatusMessage(HTTP_STATUS.Created, entityname.ToLower + ' created');
    end;
  finally
    lAR.Free;
  end;
end;

procedure TMVCActiveRecordController.UpdateEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
  lARClass: TMVCActiveRecordClass;
  lProcessor: IMVCEntityProcessor;
  lHandled: Boolean;
begin
  lProcessor := nil;
  if ActiveRecordMappingRegistry.FindProcessorByURLSegment(entityname, lProcessor) then
  begin
    lHandled := False;
    lProcessor.UpdateEntity(Context, self, entityname, id, lHandled);
    if lHandled then
    begin
      Exit;
    end;
  end;

  if not ActiveRecordMappingRegistry.FindEntityClassByURLSegment(entityname, lARClass) then
  begin
    raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Cannot find class for entity %s', [entityname]);
  end;
  lAR := lARClass.Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType), TMVCActiveRecordAction.Update) then
    begin
      Render(TMVCErrorResponse.Create(HTTP_STATUS.Forbidden, 'Cannot update ' + entityname));
      Exit;
    end;
    lAR.CheckAction(TMVCEntityAction.eaUpdate);
    if not lAR.LoadByPK(id) then
      raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Cannot find entity %s', [entityname]);
    Context.Request.BodyFor<TMVCActiveRecord>(lAR);
    lAR.SetPK(id);
    lAR.Update;
    Context.Response.CustomHeaders.Values['X-REF'] := Context.Request.PathInfo;
    if Context.Request.QueryStringParam('refresh').ToLower = 'true' then
    begin
      RenderStatusMessage(HTTP_STATUS.OK, entityname.ToLower + ' updated', '', lAR, False);
    end
    else
    begin
      RenderStatusMessage(HTTP_STATUS.OK, entityname.ToLower + ' updated');
    end;
  finally
    lAR.Free;
  end;
end;

procedure TMVCActiveRecordController.DeleteEntity(const entityname: string; const id: Integer);
var
  lAR: TMVCActiveRecord;
  lARClass: TMVCActiveRecordClass;
  lProcessor: IMVCEntityProcessor;
  lHandled: Boolean;
begin
  lProcessor := nil;
  if ActiveRecordMappingRegistry.FindProcessorByURLSegment(entityname, lProcessor) then
  begin
    lHandled := False;
    lProcessor.DeleteEntity(Context, self, entityname, id, lHandled);
    if lHandled then
    begin
      Exit;
    end;
  end;

  if not ActiveRecordMappingRegistry.FindEntityClassByURLSegment(entityname, lARClass) then
  begin
    raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Cannot find class for entity %s', [entityname]);
  end;
  lAR := lARClass.Create;
  try
    if not CheckAuthorization(TMVCActiveRecordClass(lAR.ClassType),
      TMVCActiveRecordAction.Delete) then
    begin
      Render(TMVCErrorResponse.Create(HTTP_STATUS.Forbidden, 'Cannot delete ' + entityname));
      Exit;
    end;
    {
      HTTP DELETE is an idempotent operation. Invoking it multiple times consecutively must result in
      the same behavior as the first. Meaning: you shouldn't return HTTP 404.
    }
    if lAR.LoadByPK(id) then
    begin
      lAR.SetPK(id);
      lAR.Delete;
    end;
    Render(HTTP_STATUS.OK, entityname.ToLower + ' deleted');
  finally
    lAR.Free;
  end;
end;

destructor TMVCActiveRecordController.Destroy;
begin
  inherited;
end;

{ TObjectListSetHolder }

constructor TMVCActiveRecordListResponse.Create(AList: TMVCActiveRecordList; AOwns: Boolean = True);
begin
  inherited Create;
  FOwns := AOwns;
  FList := AList;
  FMetadata := TMVCStringDictionary.Create;
end;

destructor TMVCActiveRecordListResponse.Destroy;
begin
  if FOwns then
  begin
    FList.Free
  end;
  FMetadata.Free;
  inherited;
end;

end.
