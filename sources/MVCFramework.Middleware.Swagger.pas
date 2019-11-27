// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// João Antônio Duarte (https://github.com/joaoduarte19)
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

unit MVCFramework.Middleware.Swagger;

interface

uses
  MVCFramework,
  Swag.Doc,
  MVCFramework.Swagger.Commons,
  Swag.Doc.SecurityDefinition,
  Swag.Common.Types,
  System.JSON;

type
  TMVCSwaggerMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fEngine: TMVCEngine;
    fSwaggerInfo: TMVCSwaggerInfo;
    fSwagDocURL: string;
    fJWTDescription: string;
    fEnableBasicAuthentication: Boolean;
    procedure DocumentApiInfo(const ASwagDoc: TSwagDoc);
    procedure DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
    procedure DocumentApiAuthentication(const ASwagDoc: TSwagDoc);
    procedure DocumentApi(ASwagDoc: TSwagDoc);
    procedure InternalRender(AContent: string; AContext: TWebContext);
  public
    constructor Create(const AEngine: TMVCEngine; const ASwaggerInfo: TMVCSwaggerInfo;
      const ASwaggerDocumentationURL: string = '/swagger.json'; const AJWTDescription: string = JWT_DEFAULT_DESCRIPTION;
      const AEnableBasicAuthentication: Boolean = False);
    destructor Destroy; override;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.Classes,
  JsonDataObjects,
  System.Rtti,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  MVCFramework.Middleware.JWT,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.SecurityDefinitionApiKey,
  Swag.Doc.SecurityDefinitionBasic,
  Swag.Doc.Definition;

{ TMVCSwaggerMiddleware }

constructor TMVCSwaggerMiddleware.Create(const AEngine: TMVCEngine; const ASwaggerInfo: TMVCSwaggerInfo;
  const ASwaggerDocumentationURL, AJWTDescription: string; const AEnableBasicAuthentication: Boolean);
begin
  inherited Create;
  fSwagDocURL := ASwaggerDocumentationURL;
  fEngine := AEngine;
  fSwaggerInfo := ASwaggerInfo;
  fJWTDescription := AJWTDescription;
  fEnableBasicAuthentication := AEnableBasicAuthentication;
end;

destructor TMVCSwaggerMiddleware.Destroy;
begin

  inherited Destroy;
end;

procedure TMVCSwaggerMiddleware.DocumentApi(ASwagDoc: TSwagDoc);
var
  lRttiContext: TRttiContext;
  lObjType: TRttiType;
  lController: TMVCControllerDelegate;
  lSwagPath: TSwagPath;
  lAttr: TCustomAttribute;
  lControllerPath: string;
  lMethodPath: string;
  lMethod: TRttiMethod;
  lFoundAttr: Boolean;
  lMVCHttpMethods: TMVCHTTPMethods;
  lSwagPathOp: TSwagPathOperation;
  I: TMVCHTTPMethodType;
  lPathUri: string;
  lIndex: Integer;
  lAuthTypeName: string;
  lIsIgnoredPath: Boolean;
begin
  lRttiContext := TRttiContext.Create;
  try
    for lController in fEngine.Controllers do
    begin
      lControllerPath := '';
      lObjType := lRttiContext.GetType(lController.Clazz);
      for lAttr in lObjType.GetAttributes do
      begin
        if lAttr is MVCSwagIgnorePathAttribute then
        begin
          lControllerPath := '';
          Break;
        end;
        if lAttr is MVCPathAttribute then
        begin
          lControllerPath := MVCPathAttribute(lAttr).Path;
        end;
      end;

      if lControllerPath.IsEmpty then
        Continue;

      for lMethod in lObjType.GetDeclaredMethods do
      begin
        lIsIgnoredPath := False;
        lFoundAttr := False;
        lMVCHttpMethods := [];
        lMethodPath := '';

        for lAttr in lMethod.GetAttributes do
        begin
          if lAttr is MVCSwagIgnorePathAttribute then
          begin
            lIsIgnoredPath := True;
          end;
          if lAttr is MVCPathAttribute then
          begin
            lMethodPath := MVCPathAttribute(lAttr).Path;
            lFoundAttr := True;
          end;
          if lAttr is MVCHTTPMethodsAttribute then
          begin
            lMVCHttpMethods := MVCHTTPMethodsAttribute(lAttr).MVCHTTPMethods;
          end;
        end;

        if (not lIsIgnoredPath) and lFoundAttr then
        begin
          lSwagPath := nil;
          lPathUri := TMVCSwagger.MVCPathToSwagPath(lControllerPath + lMethodPath);
          for lIndex := 0 to Pred(ASwagDoc.Paths.Count) do
          begin
            if SameText(ASwagDoc.Paths[lIndex].Uri, lPathUri) then
            begin
              lSwagPath := ASwagDoc.Paths[lIndex];
              Break;
            end;
          end;

          if not Assigned(lSwagPath) then
          begin
            lSwagPath := TSwagPath.Create;
            lSwagPath.Uri := lPathUri;
            ASwagDoc.Paths.Add(lSwagPath);
          end;

          for I in lMVCHttpMethods do
          begin
            lSwagPathOp := TSwagPathOperation.Create;
            TMVCSwagger.FillOperationSummary(lSwagPathOp, lMethod);
            if TMVCSwagger.MethodRequiresAuthentication(lMethod, lObjType, lAuthTypeName) then
            begin
              lSwagPathOp.Security.Add(lAuthTypeName);
            end;
            lSwagPathOp.Parameters.AddRange(TMVCSwagger.GetParamsFromMethod(lSwagPath.Uri, lMethod));
            lSwagPathOp.Operation := TMVCSwagger.MVCHttpMethodToSwagPathOperation(I);
            lSwagPath.Operations.Add(lSwagPathOp);
          end;
        end;
      end;
    end;
  finally
    lRttiContext.Free;
  end;
end;

procedure TMVCSwaggerMiddleware.DocumentApiInfo(const ASwagDoc: TSwagDoc);
begin
  ASwagDoc.Info.Title := fSwaggerInfo.Title;
  ASwagDoc.Info.Version := fSwaggerInfo.Version;
  ASwagDoc.Info.TermsOfService := fSwaggerInfo.TermsOfService;
  ASwagDoc.Info.Description := fSwaggerInfo.Description;
  ASwagDoc.Info.Contact.Name := fSwaggerInfo.ContactName;
  ASwagDoc.Info.Contact.Email := fSwaggerInfo.ContactEmail;
  ASwagDoc.Info.Contact.Url := fSwaggerInfo.ContactUrl;
  ASwagDoc.Info.License.Name := fSwaggerInfo.LicenseName;
  ASwagDoc.Info.License.Url := fSwaggerInfo.LicenseUrl;
end;

procedure TMVCSwaggerMiddleware.DocumentApiAuthentication(const ASwagDoc: TSwagDoc);
var
  LMiddleware: IMVCMiddleware;
  LJWTMiddleware: TMVCJWTAuthenticationMiddleware;
  LRttiContext: TRttiContext;
  LObjType: TRttiType;
  LJwtUrlField: TRttiField;
  LJwtUrlSegment: string;
  LSecurityDefsBearer: TSwagSecurityDefinitionApiKey;
  LSecurityDefsBasic: TSwagSecurityDefinitionBasic;
begin
  LJWTMiddleware := nil;
  for LMiddleware in fEngine.Middlewares do
  begin
    if LMiddleware is TMVCJWTAuthenticationMiddleware then
    begin
      LJWTMiddleware := LMiddleware as TMVCJWTAuthenticationMiddleware;
      Break;
    end;
  end;

  if Assigned(LJWTMiddleware) or fEnableBasicAuthentication then
  begin
    LSecurityDefsBasic := TSwagSecurityDefinitionBasic.Create;
    LSecurityDefsBasic.SchemeName := SECURITY_BASIC_NAME;
    LSecurityDefsBasic.Description := 'Send username and password for authentication';
    ASwagDoc.SecurityDefinitions.Add(LSecurityDefsBasic);
  end;

  if Assigned(LJWTMiddleware) then
  begin
    LRttiContext := TRttiContext.Create;
    try
      LObjType := LRttiContext.GetType(LJWTMiddleware.ClassInfo);
      LJwtUrlField := LObjType.GetField('FLoginURLSegment');
      if Assigned(LJwtUrlField) then
      begin
        LJwtUrlSegment := LJwtUrlField.GetValue(LJWTMiddleware).AsString;
        if LJwtUrlSegment.StartsWith(ASwagDoc.BasePath) then
          LJwtUrlSegment := LJwtUrlSegment.Remove(0, ASwagDoc.BasePath.Length);
        if not LJwtUrlSegment.StartsWith('/') then
          LJwtUrlSegment.Insert(0, '/');

        // Path operation Middleware JWT
        ASwagDoc.Paths.Add(TMVCSwagger.GetJWTAuthenticationPath(LJwtUrlSegment,
          LJWTMiddleware.UserNameHeaderName, LJWTMiddleware.PasswordHeaderName));

        // Methods that have the MVCRequiresAuthentication attribute use bearer authentication.
        LSecurityDefsBearer := TSwagSecurityDefinitionApiKey.Create;
        LSecurityDefsBearer.SchemeName := SECURITY_BEARER_NAME;
        LSecurityDefsBearer.InLocation := kilHeader;
        LSecurityDefsBearer.Name := 'Authorization';
        LSecurityDefsBearer.Description := fJWTDescription;
        ASwagDoc.SecurityDefinitions.Add(LSecurityDefsBearer);
      end;
    finally
      LRttiContext.Free;
    end;
  end;
end;

procedure TMVCSwaggerMiddleware.DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
begin
  ASwagDoc.Host := Format('%s:%d', [AContext.Request.RawWebRequest.Host, AContext.Request.RawWebRequest.ServerPort]);
  ASwagDoc.BasePath := fEngine.Config[TMVCConfigKey.PathPrefix];

  if ASwagDoc.BasePath.IsEmpty then
    ASwagDoc.BasePath := '/';

  ASwagDoc.Schemes := [tpsHttp, tpsHttps];
end;

procedure TMVCSwaggerMiddleware.InternalRender(AContent: string; AContext: TWebContext);
var
  LContentType: string;
  LEncoding: TEncoding;
begin
  LContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET);
  AContext.Response.RawWebResponse.ContentType := LContentType;

  LEncoding := TEncoding.GetEncoding(TMVCConstants.DEFAULT_CONTENT_CHARSET);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, LEncoding,
      TEncoding.Default.GetBytes(AContent))), LContentType);
  finally
    LEncoding.Free;
  end;
end;

procedure TMVCSwaggerMiddleware.OnAfterControllerAction(AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  //
end;

procedure TMVCSwaggerMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  //
end;

procedure TMVCSwaggerMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  LSwagDoc: TSwagDoc;
begin
  if SameText(AContext.Request.PathInfo, fSwagDocURL) and (AContext.Request.HTTPMethod in [httpGET, httpPOST]) then
  begin
    LSwagDoc := TSwagDoc.Create;
    try
      DocumentApiInfo(LSwagDoc);
      DocumentApiSettings(AContext, LSwagDoc);
      DocumentApiAuthentication(LSwagDoc);
      DocumentApi(LSwagDoc);

      LSwagDoc.GenerateSwaggerJson;
      InternalRender(LSwagDoc.SwaggerJson.Format, AContext);
      AHandled := True;

    finally
      LSwagDoc.Free;
    end;
  end;
end;

end.
