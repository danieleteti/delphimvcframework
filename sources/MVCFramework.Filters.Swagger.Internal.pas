// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
//
// INTERNAL - not an API: shared by SwaggerHTTPFilter and the deprecated
// TMVCSwaggerMiddleware; goes away with it in 4.0.

unit MVCFramework.Filters.Swagger.Internal;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Filters.Swagger,                // TMVCSwaggerAuthentications
  Swag.Doc,
  Swag.Common.Types;

type
  // A record, so that a filter closure holds it by value.
  TMVCSwaggerDocument = record
  private
    fEngine: TMVCEngine;
    fSwaggerInfo: TMVCSwaggerInfo;
    fSwagDocURL: string;
    fJWTDescription: string;
    fAuthentications: TMVCSwaggerAuthentications;
    fHost: string;
    fBasePath: string;
    fPathFilter: string;
    fTransferProtocolSchemes: TMVCTransferProtocolSchemes;
    fSpecVersion: TMVCSwaggerSpecVersion;
    procedure DocumentApiInfo(const ASwagDoc: TSwagDoc);
    procedure DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
    procedure DocumentApiAuthentication(const ASwagDoc: TSwagDoc);
    procedure DocumentApi(ASwagDoc: TSwagDoc);
    procedure DocumentActiveRecordControllerApi(ASwagDoc: TSwagDoc);
    procedure SortApiPaths(ASwagDoc: TSwagDoc);
    function SwagOperationFor(const AMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
    procedure InternalRender(AContent: string; AContext: TWebContext);
  public
    constructor Create(const AEngine: TMVCEngine;
      const AInfo: TMVCSwaggerInfo;
      const AURL: string;
      const AJWTDescription: string;
      const AAuthentications: TMVCSwaggerAuthentications;
      const AHost: string;
      const ABasePath: string;
      const APathFilter: string;
      const ATransferProtocolSchemes: TMVCTransferProtocolSchemes;
      const ASpecVersion: TMVCSwaggerSpecVersion);
    /// <summary>
    /// Host written in a Swagger 2.0 document when none is configured: the request
    /// Host, plus the server port when the Host header carries no port of its own.
    /// </summary>
    class function DocumentHost(const ARequestHost: string; const AServerPort: Integer): string; static;
    // True when AContext asks for the document (GET or POST on its URL): the response then carries it.
    function TryServe(const AContext: TWebContext): Boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  JsonDataObjects,
  System.Rtti,
  MVCFramework.Logger,
  MVCFramework.Router,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  MVCFramework.ActiveRecordController,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.SecurityDefinitionApiKey,
  Swag.Doc.SecurityDefinitionBasic,
  Swag.Doc.SecurityDefinitionHttp,
  Swag.Doc.Definition,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.TypInfo,
  System.StrUtils,
  MVCFramework.Serializer.Commons,
  Json.Common.Helpers,
  MVCFramework.ActiveRecord,
  MVCFramework.Filters.Authentication,          // TMVCJWTDefaults
  MVCFramework.Filters.Authentication.Internal; // IMVCJWTAuthenticationBundle

{ An action inherited by more controllers mapped on the same MVCPath produces the
  same URI more than once (the router serves the first one only). Documenting it
  twice would emit a duplicated operation key (e.g. two "get") under the same path. }
function SwagPathHasOperation(const ASwagPath: TSwagPath; const AOperation: TSwagPathTypeOperation;
  const AControllerName, AMethodName: string): Boolean;
var
  lSwagPathOp: TSwagPathOperation;
begin
  Result := False;
  for lSwagPathOp in ASwagPath.Operations do
  begin
    if lSwagPathOp.Operation = AOperation then
    begin
      LogW(Format('Swagger: "%s %s" is documented more than once - %s.%s has been skipped. ' +
        'Check for controllers sharing the same MVCPath',
        [UpperCase(lSwagPathOp.OperationToString), ASwagPath.Uri, AControllerName, AMethodName]));
      Exit(True);
    end;
  end;
end;

{ A sqid ("($ID:sqids)") travels as text, not as the type of the action parameter
  the router hands over. The other kinds ("($ID:int)") only check the value: the
  action parameter type stays, as in the OpenAPI 3 document. }
procedure PathConvertersAsStrings(const AMVCPath: string;
  const AParams: TObjectList<TSwagRequestParameter>);
var
  lParam: TSwagRequestParameter;
begin
  for lParam in AParams do
    if (lParam.InLocation = rpiPath) and ContainsText(AMVCPath, '($' + lParam.Name + ':sqids)') then
      lParam.TypeParameter := stpString;
end;

{ The JWT middleware is looked for by its qualified class name, matching its
  descendants as "is" did, and read through RTTI: this unit must not depend on a
  deprecated middleware unit. }
function FindJWTMiddleware(const AEngine: TMVCEngine; out AJWTClass: TClass): TObject;
const
  JWT_MIDDLEWARE_CLASS = 'MVCFramework.Middleware.JWT.TMVCJWTAuthenticationMiddleware';
var
  lMiddleware: IMVCMiddleware;
begin
  for lMiddleware in AEngine.Middlewares do
  begin
    if not (lMiddleware is TObject) then
      Continue;
    AJWTClass := (lMiddleware as TObject).ClassType;
    while Assigned(AJWTClass) and (AJWTClass.QualifiedClassName <> JWT_MIDDLEWARE_CLASS) do
      AJWTClass := AJWTClass.ClassParent;
    if Assigned(AJWTClass) then
      Exit(lMiddleware as TObject);
  end;
  AJWTClass := nil;
  Result := nil;
end;

{ TMVCSwaggerDocument }

class function TMVCSwaggerDocument.DocumentHost(const ARequestHost: string;
  const AServerPort: Integer): string;
begin
  {WebBroker (Indy bridge) gives the host name alone; Indy Direct and HTTP.sys give
   the whole Host header. "name:port" and "[v6]:port" already carry a port (the
   one the client used, which behind a proxy is not the listening one); "name"
   and "[v6]" do not. The port is looked for after the last "]".}
  if ARequestHost.Substring(ARequestHost.LastIndexOf(']') + 1).Contains(':') then
    Result := ARequestHost
  else
    Result := ARequestHost + ':' + AServerPort.ToString;
end;

constructor TMVCSwaggerDocument.Create(const AEngine: TMVCEngine;
  const AInfo: TMVCSwaggerInfo;
  const AURL: string;
  const AJWTDescription: string;
  const AAuthentications: TMVCSwaggerAuthentications;
  const AHost: string;
  const ABasePath: string;
  const APathFilter: string;
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes;
  const ASpecVersion: TMVCSwaggerSpecVersion);
begin
  fEngine := AEngine;
  fSwaggerInfo := AInfo;
  fSwagDocURL := AURL;
  fJWTDescription := AJWTDescription;
  fAuthentications := AAuthentications;
  fHost := AHost;
  fBasePath := ABasePath;
  fPathFilter := APathFilter;
  fTransferProtocolSchemes := ATransferProtocolSchemes;
  fSpecVersion := ASpecVersion;
end;

procedure TMVCSwaggerDocument.DocumentActiveRecordControllerApi(ASwagDoc: TSwagDoc);
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
  lVisitedMethodSignatures: TList<String>;
  lMethodSignature: string;
  lControllerDefaultModelSingularName: string;
  lControllerDefaultModelPluralName: string;
  lEntitiesMapping: TArray<TMVCEntityMapping>;
  lPrefixURLSegment: string;
  lEntityMapping: TMVCEntityMapping;
begin
  lVisitedMethodSignatures := TList<String>.Create;
  try
    lRttiContext := TRttiContext.Create;
    try
      for lController in fEngine.Controllers do
      begin
        lControllerPath := '';
        SetLength(lControllerDefaultSummaryTags, 0);
        lObjType := lRttiContext.GetType(lController.Clazz);

        //Automatic API generated by TMVCActiveRecordController
        if not lController.Clazz.InheritsFrom(TMVCActiveRecordController) then
        begin
          Continue;
        end;

        lEntitiesMapping := ActiveRecordMappingRegistry.GetURLSegmentWithEntities;
        lPrefixURLSegment := lController.URLSegment;

        for lEntityMapping in lEntitiesMapping do
        begin
          lControllerPath := lPrefixURLSegment;
          lControllerDefaultModelClass := lEntityMapping.Value;
          lControllerDefaultModelSingularName := lEntityMapping.Value.ClassName;
          lControllerDefaultModelPluralName := lEntityMapping.Key;
          SetLength(lControllerDefaultSummaryTags, 1);
          lControllerDefaultSummaryTags[0] := TMVCSerializerHelper.ApplyNameCase(ncPascalCase, lEntityMapping.Key);

          for lMethod in lObjType.GetMethods do
          begin
            {only public and published methods are inspected}
            if not (lMethod.Visibility in [mvPublished, mvPublic]) then
            begin
              Continue;
            end;

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
                lMethodPath := lMethodPath.Replace('($entityname)', lEntityMapping.Key, [rfReplaceAll]);
                lFoundAttr := True;
              end;
            end;
            {Same answer the router gives: the union of every [MVCHTTPMethod],
             or all the verbs when the action declares none}
            lMVCHttpMethods := TMVCRouter.AllowedMethods(lMethod.GetAttributes);

            if (not lIsIgnoredPath) and lFoundAttr then
            begin
              lMethodSignature := lObjType.Name + '.' + lEntityMapping.Key + '_' + lMethod.Name;
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

              for I in lMVCHttpMethods do
              begin
                {A verb the document cannot express (QUERY in Swagger 2.0) comes back as
                 ohvNotDefined: emitting it would write an empty operation key}
                if SwagOperationFor(I) = ohvNotDefined then
                begin
                  Continue;
                end;
                if Assigned(lSwagPath) and SwagPathHasOperation(lSwagPath, SwagOperationFor(I),
                  lObjType.Name, lMethod.Name) then
                begin
                  Continue;
                end;
                {created on the first verb the document can express: a path with no
                 operation is what made the sort of the paths fail}
                if not Assigned(lSwagPath) then
                begin
                  lSwagPath := TSwagPath.Create;
                  try
                    lSwagPath.Uri := lPathUri;
                    ASwagDoc.Paths.Add(lSwagPath);
                  except
                    lSwagPath.Free;
                    raise;
                  end;
                end;
                lSwagPathOp := TSwagPathOperation.Create;
                try
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
                      I,
                      lControllerDefaultModelClass,
                      lControllerDefaultModelSingularName,
                      lControllerDefaultModelPluralName)
                    );
                  PathConvertersAsStrings(lControllerPath + lMethodPath, lSwagPathOp.Parameters);
                  lSwagPathOp.Operation := SwagOperationFor(I);
                  lSwagPath.Operations.Add(lSwagPathOp);
                  lSwagPathOp := nil; // ownership transferred to lSwagPath.Operations
                except
                  lSwagPathOp.Free;
                  raise;
                end;
              end;
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

procedure TMVCSwaggerDocument.DocumentApi(ASwagDoc: TSwagDoc);
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
          end;
          {Same answer the router gives: the union of every [MVCHTTPMethod],
           or all the verbs when the action declares none}
          lMVCHttpMethods := TMVCRouter.AllowedMethods(lMethod.GetAttributes);

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

            for I in lMVCHttpMethods do
            begin
              {A verb the document cannot express (QUERY in Swagger 2.0) comes back as
                 ohvNotDefined: emitting it would write an empty operation key}
              if SwagOperationFor(I) = ohvNotDefined then
              begin
                Continue;
              end;
              if Assigned(lSwagPath) and SwagPathHasOperation(lSwagPath, SwagOperationFor(I),
                lObjType.Name, lMethod.Name) then
              begin
                Continue;
              end;
              {created on the first verb the document can express: a path with no
               operation is what made the sort of the paths fail}
              if not Assigned(lSwagPath) then
              begin
                lSwagPath := TSwagPath.Create;
                try
                  lSwagPath.Uri := lPathUri;
                  ASwagDoc.Paths.Add(lSwagPath);
                except
                  lSwagPath.Free;
                  raise;
                end;
              end;
              lSwagPathOp := TSwagPathOperation.Create;
              try
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
                    I,
                    lControllerDefaultModelClass,
                    lControllerDefaultModelSingularName,
                    lControllerDefaultModelPluralName)
                  );
                PathConvertersAsStrings(lControllerPath + lMethodPath, lSwagPathOp.Parameters);
                lSwagPathOp.Operation := SwagOperationFor(I);
                if lSwagPathOp.OperationID.IsEmpty then
                begin
                  {Named after the concrete controller, not after the class declaring the
                   method: two controllers inheriting the same action need distinct ids.}
                  lSwagPathOp.OperationID := lObjType.Name + '_' + lMethod.Name + '_' +
                    lSwagPathOp.OperationToString;
                end;
                lSwagPath.Operations.Add(lSwagPathOp);
                lSwagPathOp := nil; // ownership transferred to lSwagPath.Operations
              except
                lSwagPathOp.Free;
                raise;
              end;
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

procedure TMVCSwaggerDocument.DocumentApiInfo(const ASwagDoc: TSwagDoc);
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


procedure TMVCSwaggerDocument.DocumentApiAuthentication(const ASwagDoc: TSwagDoc);
var
  lJWTMiddleware: TObject;
  lJWTClass: TClass;
  lRttiContext: TRttiContext;
  lObjType: TRttiType;
  lJWTType: TRttiType;
  lJwtUrlProp: TRttiProperty;
  lBundle: IMVCFilterBundle;
  lJWTBundle: IMVCJWTAuthenticationBundle;
  lHasJWTBundle: Boolean;
  lHasJWTBundleLogin: Boolean;
  lSecurityDefsBearer: TSwagSecurityDefinition;
  lSecurityDefsBasic: TSwagSecurityDefinitionBasic;

  procedure AddJWTLoginPath(ALoginURL: string; const AUserNameHeaderName, APasswordHeaderName: string);
  var
    lIndex: Integer;
  begin
    if ALoginURL.StartsWith(ASwagDoc.BasePath) then
      ALoginURL := ALoginURL.Remove(0, ASwagDoc.BasePath.Length);
    if not ALoginURL.StartsWith('/') then
      ALoginURL := '/' + ALoginURL;
    // more bundles on the same login URL: the first one answers it
    for lIndex := 0 to Pred(ASwagDoc.Paths.Count) do
      if SameText(ASwagDoc.Paths[lIndex].Uri, ALoginURL) then
        Exit;
    ASwagDoc.Paths.Add(TMVCSwagger.GetJWTAuthenticationPath(ALoginURL, AUserNameHeaderName, APasswordHeaderName));
  end;

begin
  lJwtUrlProp := nil;
  lJWTMiddleware := FindJWTMiddleware(fEngine, lJWTClass);
  if Assigned(lJWTMiddleware) then
  begin
    lRttiContext := TRttiContext.Create;
    try
      lObjType := lRttiContext.GetType(lJWTMiddleware.ClassInfo);
      lJwtUrlProp := lObjType.GetProperty('LoginURLSegment');
      if Assigned(lJwtUrlProp) then
      begin
        // Path operation Middleware JWT
        lJWTType := lRttiContext.GetType(lJWTClass);
        AddJWTLoginPath(lJwtUrlProp.GetValue(lJWTMiddleware).AsString,
          lJWTType.GetProperty('UserNameHeaderName').GetValue(lJWTMiddleware).AsString,
          lJWTType.GetProperty('PasswordHeaderName').GetValue(lJWTMiddleware).AsString);
      end;
    finally
      lRttiContext.Free;
    end;
  end;

  // JWTAuthenticationBundle, engine-wide or on a route group: its login is on the engine either way
  lHasJWTBundle := False;
  lHasJWTBundleLogin := False;
  for lBundle in fEngine.FilterBundles do
    if Supports(lBundle, IMVCJWTAuthenticationBundle, lJWTBundle) then
    begin
      lHasJWTBundle := True;
      if lJWTBundle.LoginURL <> '' then
      begin
        lHasJWTBundleLogin := True;
        AddJWTLoginPath(lJWTBundle.LoginURL, TMVCJWTDefaults.USERNAME_HEADER, TMVCJWTDefaults.PASSWORD_HEADER);
      end;
    end;

  // the JWT login operation refers to it (and MVCSwagAuthentication(atBasic), with sauBasic)
  if Assigned(lJWTMiddleware) or lHasJWTBundleLogin or (sauBasic in fAuthentications) then
  begin
    lSecurityDefsBasic := TSwagSecurityDefinitionBasic.Create;
    try
      lSecurityDefsBasic.SchemeName := SECURITY_BASIC_NAME;
      lSecurityDefsBasic.Description := 'Send username and password for authentication';
      ASwagDoc.SecurityDefinitions.Add(lSecurityDefsBasic);
    except
      lSecurityDefsBasic.Free;
      raise;
    end;
  end;

  // Methods that have the MVCRequiresAuthentication attribute use bearer authentication.
  if (sauBearer in fAuthentications) or lHasJWTBundle or
   (Assigned(lJWTMiddleware) and Assigned(lJwtUrlProp)) then
  begin
    {OpenAPI 3 has a real bearer scheme: Swagger UI then takes the raw token and
     sends "Authorization: Bearer <token>". Swagger 2.0 has none, so there the
     scheme stays an API key in the Authorization header (the value typed must
     include "Bearer ").}
    if fSpecVersion = ssvOpenAPI3 then
    begin
      lSecurityDefsBearer := TSwagSecurityDefinitionHttp.Create;
      TSwagSecurityDefinitionHttp(lSecurityDefsBearer).Scheme := 'bearer';
      TSwagSecurityDefinitionHttp(lSecurityDefsBearer).BearerFormat := 'JWT';
    end
    else
    begin
      lSecurityDefsBearer := TSwagSecurityDefinitionApiKey.Create;
      TSwagSecurityDefinitionApiKey(lSecurityDefsBearer).InLocation := kilHeader;
      TSwagSecurityDefinitionApiKey(lSecurityDefsBearer).Name := 'Authorization';
    end;
    try
      lSecurityDefsBearer.SchemeName := SECURITY_BEARER_NAME;
      lSecurityDefsBearer.Description := fJWTDescription;
      ASwagDoc.SecurityDefinitions.Add(lSecurityDefsBearer);
    except
      lSecurityDefsBearer.Free;
      raise;
    end;
  end;
end;

procedure TMVCSwaggerDocument.DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
var
  lSwagSchemes: TSwagTransferProtocolSchemes;
begin
  ASwagDoc.Host := fHost;
  // OpenAPI 3 without a configured host: no host at all, so SwagDoc writes a
  // relative server ("servers": [{"url": <basePath>}]) that clients resolve
  // against the URL the document came from. The request Host header is not
  // echoed into the document.
  if ASwagDoc.Host.IsEmpty and (fSpecVersion = ssvSwagger2) then
  begin
    ASwagDoc.Host := DocumentHost(AContext.Request.Host, AContext.Request.ServerPort);
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

procedure TMVCSwaggerDocument.InternalRender(AContent: string; AContext: TWebContext);
var
  lContentType: String;
begin
  lContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharSet.UTF_8);
  AContext.Response.ContentType := LContentType;
  AContext.Response.SetContentStream(TStringStream.Create(AContent, TEncoding.UTF8), lContentType);
end;

function TMVCSwaggerDocument.TryServe(const AContext: TWebContext): Boolean;
var
  LSwagDoc: TSwagDoc;
begin
  Result := SameText(AContext.Request.PathInfo, fSwagDocURL) and (AContext.Request.HTTPMethod in [httpGET, httpPOST]);
  if not Result then
    Exit;
  LSwagDoc := TSwagDoc.Create;
  try
    if fSpecVersion = ssvOpenAPI3 then
      LSwagDoc.SpecVersion := svOpenApi3;
    DocumentApiInfo(LSwagDoc);
    DocumentApiSettings(AContext, LSwagDoc);
    DocumentApiAuthentication(LSwagDoc);
    DocumentActiveRecordControllerApi(LSwagDoc);
    DocumentApi(LSwagDoc);
    SortApiPaths(LSwagDoc);

    LSwagDoc.GenerateSwaggerJson;
    InternalRender(LSwagDoc.SwaggerJson.Format, AContext);
  finally
    LSwagDoc.Free;
  end;
end;

function SwagPathSortRank(const APath: TSwagPath): Integer;
begin
  if (APath.Operations.Count = 0) or (APath.Operations[0].Tags.Count = 0) then
    Result := 2 // untagged last
  else if SameText(APath.Operations[0].Tags[0], JWT_AUTHENTICATION_TAG) then
    Result := 0 // the JWT login first
  else
    Result := 1;
end;

function TMVCSwaggerDocument.SwagOperationFor(const AMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
begin
  // OpenAPI 3.2 has a "query" operation; Swagger 2.0 has none, so there it stays undefined
  if (AMethod = httpQUERY) and (fSpecVersion = ssvOpenAPI3) then
    Result := ohvQuery
  else
    Result := TMVCSwagger.MVCHttpMethodToSwagPathOperation(AMethod);
end;

procedure TMVCSwaggerDocument.SortApiPaths(ASwagDoc: TSwagDoc);
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
      {a total order, as QuickSort needs: the old comparer answered 1 to both
       (A, B) and (B, A) for an untagged path and read past the list end}
      Result := SwagPathSortRank(Left) - SwagPathSortRank(Right);
      if (Result = 0) and (SwagPathSortRank(Left) = 1) then
        Result := CompareText(Left.Operations[0].Tags[0], Right.Operations[0].Tags[0]);
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
