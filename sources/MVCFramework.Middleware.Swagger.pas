// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  Swag.Doc,
  MVCFramework.Swagger.Commons,
  Swag.Doc.SecurityDefinition,
  Swag.Common.Types,
  System.JSON, MVCFramework.Commons;

type
  TMVCSwaggerMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fEngine: TMVCEngine;
    fSwaggerInfo: TMVCSwaggerInfo;
    fSwagDocURL: string;
    fJWTDescription: string;
    fEnableBasicAuthentication: Boolean;
    fHost: string;
    fBasePath: string;
    fPathFilter: string;
    fTransferProtocolSchemes: TMVCTransferProtocolSchemes;
    procedure DocumentApiInfo(const ASwagDoc: TSwagDoc);
    procedure DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
    procedure DocumentApiAuthentication(const ASwagDoc: TSwagDoc);
    procedure DocumentApi(ASwagDoc: TSwagDoc);
    procedure SortApiPaths(ASwagDoc: TSwagDoc);
    procedure InternalRender(AContent: string; AContext: TWebContext);
  public
    constructor Create(
      const AEngine: TMVCEngine;
      const ASwaggerInfo: TMVCSwaggerInfo;
      const ASwaggerDocumentationURL: string = '/swagger.json';
      const AJWTDescription: string = JWT_DEFAULT_DESCRIPTION;
      const AEnableBasicAuthentication: Boolean = False;
      const AHost: string = '';
      const ABasePath: string = '';
      const APathFilter: String = '';
      const ATransferProtocolSchemes: TMVCTransferProtocolSchemes = [psHTTP, psHTTPS]);
    destructor Destroy; override;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  System.SysUtils,
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
  Swag.Doc.Definition,
  System.Generics.Collections,
  System.Generics.Defaults, 
  System.TypInfo,
  Json.Common.Helpers;

{ TMVCSwaggerMiddleware }

constructor TMVCSwaggerMiddleware.Create(const AEngine: TMVCEngine; const ASwaggerInfo: TMVCSwaggerInfo;
  const ASwaggerDocumentationURL, AJWTDescription: string; const AEnableBasicAuthentication: Boolean;
  const AHost, ABasePath: string;
  const APathFilter: String;
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes);
begin
  inherited Create;
  fSwagDocURL := ASwaggerDocumentationURL;
  fEngine := AEngine;
  fSwaggerInfo := ASwaggerInfo;
  fJWTDescription := AJWTDescription;
  fEnableBasicAuthentication := AEnableBasicAuthentication;
  fHost := AHost;
  fBasePath := ABasePath;
  fPathFilter := APathFilter;
  fTransferProtocolSchemes := ATransferProtocolSchemes;
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
  lControllerDefaultModelClass: TClass;
  lControllerDefaultSummaryTags: TArray<string>;
  lPathAttributeFound: Boolean;
  lVisitedMethodSignatures: TList<String>;
  lMethodSignature: string;
  lControllerDefaultModelSingularName: string;
  lControllerDefaultModelPluralName: string;
begin
  lVisitedMethodSignatures := TList<String>.Create;
  try
    lRttiContext := TRttiContext.Create;
    try
      for lController in fEngine.Controllers do
      begin
        lControllerDefaultModelClass := nil;
        lControllerPath := '';
        SetLength(lControllerDefaultSummaryTags, 0);
        lPathAttributeFound := False;
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
            if not lPathAttributeFound then
            begin
              {in case of more than one MVCPath attribute, only the firstone
              is considered by swagger}
              lControllerPath := MVCPathAttribute(lAttr).Path;
              lPathAttributeFound := fPathFilter.IsEmpty or lControllerPath.StartsWith(fPathFilter);
            end;
          end;
          if lAttr is MVCSWAGDefaultModel then
          begin
            lControllerDefaultModelClass := MVCSWAGDefaultModel(lAttr).JsonSchemaClass;
            lControllerDefaultModelSingularName := MVCSWAGDefaultModel(lAttr).SingularModelName;
            lControllerDefaultModelPluralName := MVCSWAGDefaultModel(lAttr).PluralModelName;
          end;
          if lAttr is MVCSWAGDefaultSummaryTags then
          begin
            lControllerDefaultSummaryTags := MVCSWAGDefaultSummaryTags(lAttr).GetTags;
          end;
        end;

        if not lPathAttributeFound then
          Continue;

        //for lMethod in lObjType.GetDeclaredMethods do
        for lMethod in lObjType.GetMethods do
        begin
          {only public and puches methods are inspected}
          if not (lMethod.Visibility in [mvPublished, mvPublic]) then
          begin
            continue;
          end;

          {here could arrive also overwritten methods, so we need to exclude
           method which have been overwritten. We can do this checking if the method class
           is the controller which we are inspecting }
  //        if lObjType <> lMethod.Parent then
  //        begin
  //          Continue;
  //        end;

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
            lMethodSignature := lObjType.Name + '.' + lMethod.Name;
            if lVisitedMethodSignatures.Contains(lMethodSignature) then
            begin
              Continue;
            end
            else
            begin
              lVisitedMethodSignatures.Add(lMethodSignature);
            end;

            //LogI(lObjType.Name + '.' + lMethod.Name + ' ' + lMethod.Parent.ToString);
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
              TMVCSwagger.FillOperationSummary(
                lSwagPathOp,
                lMethod,
                ASwagDoc.Definitions,
                I,
                lControllerDefaultModelClass,
                lControllerDefaultModelSingularName,
                lControllerDefaultModelPluralName,
                lControllerDefaultSummaryTags);
              if TMVCSwagger.MethodRequiresAuthentication(lMethod, lObjType, lAuthTypeName) then
              begin
                lSwagPathOp.Security.Add(lAuthTypeName);
              end;
              lSwagPathOp.Parameters.AddRange(
                TMVCSwagger.GetParamsFromMethod(
                  lSwagPath.Uri,
                  lMethod,
                  ASwagDoc.Definitions,
                  lControllerDefaultModelClass,
                  lControllerDefaultModelSingularName,
                  lControllerDefaultModelPluralName)
                );
              lSwagPathOp.Operation := TMVCSwagger.MVCHttpMethodToSwagPathOperation(I);
              lSwagPath.Operations.Add(lSwagPathOp);
            end;
          end;
        end;
      end;
    finally
      lRttiContext.Free;
    end;
  finally
    lVisitedMethodSignatures.Free;
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
  lMiddleware: IMVCMiddleware;
  lJWTMiddleware: TMVCJWTAuthenticationMiddleware;
  lRttiContext: TRttiContext;
  lObjType: TRttiType;
  lJwtUrlField: TRttiField;
  lJwtUrlSegment: string;
  lSecurityDefsBearer: TSwagSecurityDefinitionApiKey;
  lSecurityDefsBasic: TSwagSecurityDefinitionBasic;
begin
  lJWTMiddleware := nil;
  for lMiddleware in fEngine.Middlewares do
  begin
    if lMiddleware is TMVCJWTAuthenticationMiddleware then
    begin
      lJWTMiddleware := lMiddleware as TMVCJWTAuthenticationMiddleware;
      Break;
    end;
  end;

  if Assigned(lJWTMiddleware) or fEnableBasicAuthentication then
  begin
    lSecurityDefsBasic := TSwagSecurityDefinitionBasic.Create;
    lSecurityDefsBasic.SchemeName := SECURITY_BASIC_NAME;
    lSecurityDefsBasic.Description := 'Send username and password for authentication';
    ASwagDoc.SecurityDefinitions.Add(lSecurityDefsBasic);
  end;

  if Assigned(lJWTMiddleware) then
  begin
    lRttiContext := TRttiContext.Create;
    try
      lObjType := lRttiContext.GetType(lJWTMiddleware.ClassInfo);
      lJwtUrlField := lObjType.GetField('FLoginURLSegment');
      if Assigned(lJwtUrlField) then
      begin
        lJwtUrlSegment := lJwtUrlField.GetValue(lJWTMiddleware).AsString;
        if lJwtUrlSegment.StartsWith(ASwagDoc.BasePath) then
          lJwtUrlSegment := lJwtUrlSegment.Remove(0, ASwagDoc.BasePath.Length);
        if not lJwtUrlSegment.StartsWith('/') then
          lJwtUrlSegment.Insert(0, '/');

        // Path operation Middleware JWT
        ASwagDoc.Paths.Add(TMVCSwagger.GetJWTAuthenticationPath(lJwtUrlSegment,
          lJWTMiddleware.UserNameHeaderName, lJWTMiddleware.PasswordHeaderName));

        // Methods that have the MVCRequiresAuthentication attribute use bearer authentication.
        lSecurityDefsBearer := TSwagSecurityDefinitionApiKey.Create;
        lSecurityDefsBearer.SchemeName := SECURITY_BEARER_NAME;
        lSecurityDefsBearer.InLocation := kilHeader;
        lSecurityDefsBearer.Name := 'Authorization';
        lSecurityDefsBearer.Description := fJWTDescription;
        ASwagDoc.SecurityDefinitions.Add(lSecurityDefsBearer);
      end;
    finally
      lRttiContext.Free;
    end;
  end;
end;

procedure TMVCSwaggerMiddleware.DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
var
  lSwagSchemes: TSwagTransferProtocolSchemes;
begin
  ASwagDoc.Host := fHost;
  if ASwagDoc.Host.IsEmpty then
  begin
    ASwagDoc.Host := Format('%s:%d', [AContext.Request.RawWebRequest.Host, AContext.Request.RawWebRequest.ServerPort]);
  end;

  ASwagDoc.BasePath := fBasePath;
  if ASwagDoc.BasePath.IsEmpty then
  begin
    ASwagDoc.BasePath := fEngine.Config[TMVCConfigKey.PathPrefix];
  end;
  if ASwagDoc.BasePath.IsEmpty then
  begin
    ASwagDoc.BasePath := '/';
  end;

  lSwagSchemes := [];
  if psHTTP in fTransferProtocolSchemes then
  begin
    Include(lSwagSchemes, tpsHttp);
  end;
  if psHTTPS in fTransferProtocolSchemes then
  begin
    Include(lSwagSchemes, tpsHttps);
  end;
  ASwagDoc.Schemes := lSwagSchemes;
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

procedure TMVCSwaggerMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSwaggerMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSwaggerMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  // do nothing
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
      SortApiPaths(LSwagDoc);

      LSwagDoc.GenerateSwaggerJson;
      InternalRender(LSwagDoc.SwaggerJson.Format, AContext);
      AHandled := True;

    finally
      LSwagDoc.Free;
    end;
  end;
end;

procedure TMVCSwaggerMiddleware.SortApiPaths(ASwagDoc: TSwagDoc);
var
  lPathComparer: IComparer<TSwagPath>;
  lOperationComparer: IComparer<TSwagPathOperation>;
  lSwagPath: TSwagPath;
{$IF not defined(RIOORBETTER)}
  lSwagPathList: TArray<TSwagPath>;
  lSwagOperationList: TArray<TSwagPathOperation>;
{$ENDIF}
begin
  // Sort paths
  lPathComparer := TDelegatedComparer<TSwagPath>.Create(
    function(const Left, Right: TSwagPath): Integer
    begin
      if (Left.Operations.Count = 0) or (Left.Operations[0].Tags.Count = 0) or
        (Right.Operations.Count = 0) or (Right.Operations[0].Tags.Count = 0) then
      begin
        Result := 1;
      end
      else if SameText(Left.Operations[0].Tags[0], JWT_AUTHENTICATION_TAG) or
        SameText(Right.Operations[0].Tags[0], JWT_AUTHENTICATION_TAG) then
      begin
        Result := -1;
      end
      else
      begin
        Result := CompareText(Left.Operations[0].Tags[0], Right.Operations[0].Tags[0]);
      end;
    end);

{$IF defined(RIOORBETTER)}
  ASwagDoc.Paths.Sort(lPathComparer);
{$ELSE}
  ASwagDoc.Paths.TrimExcess;
  lSwagPathList := ASwagDoc.Paths.ToArray;
  ASwagDoc.Paths.OwnsObjects := False;
  ASwagDoc.Paths.Clear;
  TArrayHelper.QuickSort<TSwagPath>(lSwagPathList, lPathComparer, Low(lSwagPathList), High(lSwagPathList));
  ASwagDoc.Paths.AddRange(lSwagPathList);
  ASwagDoc.Paths.OwnsObjects := True;
{$ENDIF}

  // Sort paths operations
  lOperationComparer := TDelegatedComparer<TSwagPathOperation>.Create(
  function(const Left, Right: TSwagPathOperation): Integer
  begin
    if Ord(Left.Operation) > Ord(Right.Operation) then
      Result := -1
    else if Ord(Left.Operation) < Ord(Right.Operation) then
      Result := 1
    else
      Result := 0;
  end);

  for lSwagPath in ASwagDoc.Paths do
  begin
{$IF defined(RIOORBETTER)}
    lSwagPath.Operations.Sort(lOperationComparer);
{$ELSE}
    lSwagPath.Operations.TrimExcess;
    lSwagOperationList := lSwagPath.Operations.ToArray;
    lSwagPath.Operations.OwnsObjects := False;
    lSwagPath.Operations.Clear;
    TArrayHelper.QuickSort<TSwagPathOperation>(lSwagOperationList, lOperationComparer,
      Low(lSwagOperationList), High(lSwagOperationList));
    lSwagPath.Operations.AddRange(lSwagOperationList);
    lSwagPath.Operations.OwnsObjects := True;
{$ENDIF}
  end;

end;

end.
