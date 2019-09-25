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
    FEngine: TMVCEngine;
    FSwaggerInfo: TMVCSwaggerInfo;
    FSwagDocURL: string;
    procedure DocumentApiInfo(const ASwagDoc: TSwagDoc);
    procedure DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
    procedure DocumentApiJWTAuthentication(const ASwagDoc: TSwagDoc);
    procedure DocumentApi(ASwagDoc: TSwagDoc);
    procedure InternalRender(AContent: string; AContext: TWebContext);
    procedure RenderError(const AContext: TWebContext; const AErrorMessage, AErrorClassName: string);
  public
    constructor Create(
      const AEngine: TMVCEngine;
      const ASwaggerInfo: TMVCSwaggerInfo;
      const ASwaggerDocumentationURL: string = '/swagger.json'
      );
    destructor Destroy; override;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
  end;

  TSwagSecurityDefinitionBasic = class(TSwagSecurityDefinition)
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    function GenerateJsonObject: System.JSON.TJSONObject; override;
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
  Swag.Doc.SecurityDefinitionApiKey;

{ TMVCSwaggerMiddleware }

constructor TMVCSwaggerMiddleware.Create(const AEngine: TMVCEngine; const ASwaggerInfo: TMVCSwaggerInfo;
  const ASwaggerDocumentationURL: string);
begin
  inherited Create;
  FSwagDocURL := ASwaggerDocumentationURL;
  FEngine := AEngine;
  FSwaggerInfo := ASwaggerInfo;
end;

destructor TMVCSwaggerMiddleware.Destroy;
begin

  inherited Destroy;
end;

procedure TMVCSwaggerMiddleware.DocumentApi(ASwagDoc: TSwagDoc);
var
  LRttiContext: TRttiContext;
  LObjType: TRttiType;
  LController: TMVCControllerDelegate;
  LSwagPath: TSwagPath;
  LAttr: TCustomAttribute;
  LControllerPath: string;
  LMethodPath: string;
  LMethod: TRttiMethod;
  LFoundAttr: Boolean;
  LMVCHttpMethods: TMVCHTTPMethods;
  LSwagPathOp: TSwagPathOperation;
  I: TMVCHTTPMethodType;
  LPathUri: string;
  LIndex: Integer;
begin
  LRttiContext := TRttiContext.Create;
  try
    for LController in FEngine.Controllers do
    begin
      LControllerPath := '';
      LObjType := LRttiContext.GetType(LController.Clazz);
      for LAttr in LObjType.GetAttributes do
      begin
        if LAttr is MVCPathAttribute then
        begin
          LControllerPath := MVCPathAttribute(LAttr).Path;
          Break;
        end;
      end;

      if LControllerPath.IsEmpty then
        Continue;

      for LMethod in LObjType.GetDeclaredMethods do
      begin
        LFoundAttr := False;
        LMVCHttpMethods := [];
        LMethodPath := '';

        for LAttr in LMethod.GetAttributes do
        begin
          if LAttr is MVCPathAttribute then
          begin
            LMethodPath := MVCPathAttribute(LAttr).Path;
            LFoundAttr := True;
          end;
          if LAttr is MVCHTTPMethodsAttribute then
          begin
            LMVCHttpMethods := MVCHTTPMethodsAttribute(LAttr).MVCHTTPMethods;
          end;
        end;

        if LFoundAttr then
        begin
          LSwagPath := nil;
          LPathUri := TMVCSwagger.MVCPathToSwagPath(LControllerPath + LMethodPath);
          for LIndex := 0 to Pred(ASwagDoc.Paths.Count) do
          begin
            if SameText(ASwagDoc.Paths[LIndex].Uri, LPathUri) then
            begin
              LSwagPath := ASwagDoc.Paths[LIndex];
              Break;
            end;
          end;

          if not Assigned(LSwagPath) then
          begin
            LSwagPath := TSwagPath.Create;
            LSwagPath.Uri := LPathUri;
            ASwagDoc.Paths.Add(LSwagPath);
          end;

          for I in LMVCHttpMethods do
          begin
            LSwagPathOp := TSwagPathOperation.Create;
            TMVCSwagger.FillOperationSummary(LSwagPathOp, LMethod);

            if TMVCSwagger.MethodRequiresAuthentication(LMethod, LObjType) then
              LSwagPathOp.Security.Add(SECURITY_BEARER_NAME);

            LSwagPathOp.Parameters.AddRange(TMVCSwagger.GetParamsFromMethod(LSwagPath.Uri, LMethod));
            LSwagPathOp.Operation := TMVCSwagger.MVCHttpMethodToSwagPathOperation(I);
            LSwagPath.Operations.Add(LSwagPathOp);
          end;
        end;
      end;
    end;
  finally
    LRttiContext.Free;
  end;
end;

procedure TMVCSwaggerMiddleware.DocumentApiInfo(const ASwagDoc: TSwagDoc);
begin
  ASwagDoc.Info.Title := FSwaggerInfo.Title;
  ASwagDoc.Info.Version := FSwaggerInfo.Version;
  ASwagDoc.Info.TermsOfService := FSwaggerInfo.TermsOfService;
  ASwagDoc.Info.Description := FSwaggerInfo.Description;
  ASwagDoc.Info.Contact.Name := FSwaggerInfo.ContactName;
  ASwagDoc.Info.Contact.Email := FSwaggerInfo.ContactEmail;
  ASwagDoc.Info.Contact.Url := FSwaggerInfo.ContactUrl;
  ASwagDoc.Info.License.Name := FSwaggerInfo.LicenseName;
  ASwagDoc.Info.License.Url := FSwaggerInfo.LicenseUrl;
end;

procedure TMVCSwaggerMiddleware.DocumentApiJWTAuthentication(const ASwagDoc: TSwagDoc);
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
  for LMiddleware in FEngine.Middlewares do
  begin
    if LMiddleware is TMVCJWTAuthenticationMiddleware then
    begin
      LJWTMiddleware := LMiddleware as TMVCJWTAuthenticationMiddleware;
      Break;
    end;
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
        ASwagDoc.Paths.Add(TMVCSwagger.GetJWTAuthenticationPath(LJwtUrlSegment));

        // basic auth is used by jwt middleware to generate json web token
        LSecurityDefsBasic := TSwagSecurityDefinitionBasic.Create;
        LSecurityDefsBasic.SchemaName := SECURITY_BASIC_NAME;
        LSecurityDefsBasic.Description := 'Send UserName and Password to return JWT Token';
        ASwagDoc.SecurityDefinitions.Add(LSecurityDefsBasic);

        // Methods that have the MVCRequiresAuthentication attribute use bearer authentication.
        LSecurityDefsBearer := TSwagSecurityDefinitionApiKey.Create;
        LSecurityDefsBearer.SchemaName := SECURITY_BEARER_NAME;
        LSecurityDefsBearer.InLocation := kilHeader;
        LSecurityDefsBearer.Name := 'Authorization';
        LSecurityDefsBearer.Description :=
          'For accessing the API a valid JWT token must be passed in all the queries ' +
          'in the ''Authorization'' header.' + sLineBreak + sLineBreak +
          'A valid JWT token is generated by the API and retourned as answer of a call ' +
          'to the route `' + LJwtUrlSegment + '` giving a valid username and password.' + sLineBreak + sLineBreak +
          'The following syntax must be used in the ''Authorization'' header :' + sLineBreak + sLineBreak +
          '    Bearer xxxxxx.yyyyyyy.zzzzzz' + sLineBreak;
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
  ASwagDoc.BasePath := FEngine.Config[TMVCConfigKey.PathPrefix];

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

procedure TMVCSwaggerMiddleware.OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  //
end;

procedure TMVCSwaggerMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  LSwagDoc: TSwagDoc;
begin
  if SameText(AContext.Request.PathInfo, FSwagDocURL) and (AContext.Request.HTTPMethod in [httpGET, httpPOST]) then
  begin
    LSwagDoc := TSwagDoc.Create;
    try
      try
        DocumentApiInfo(LSwagDoc);
        DocumentApiSettings(AContext, LSwagDoc);
        DocumentApiJWTAuthentication(LSwagDoc);
        DocumentApi(LSwagDoc);

        LSwagDoc.GenerateSwaggerJson;
        InternalRender(LSwagDoc.SwaggerJson.Format, AContext);
        AHandled := True;
      except
        on E: Exception do
        begin
          RenderError(AContext, E.Message, E.ClassName);
          AHandled := True;
        end;
      end;
    finally
      LSwagDoc.Free;
    end;
  end;
end;

procedure TMVCSwaggerMiddleware.RenderError(const AContext: TWebContext;
  const AErrorMessage, AErrorClassName: string);
var
  LJSonOb: TJDOJsonObject;
begin
  AContext.Response.StatusCode := HTTP_STATUS.InternalServerError;
  AContext.Response.ReasonString := AErrorMessage;

  LJSonOb := TJDOJsonObject.Create;
  try
    LJSonOb.S['status'] := 'error';

    if AErrorClassName = '' then
      LJSonOb.Values['classname'] := nil
    else
      LJSonOb.S['classname'] := AErrorClassName;

    LJSonOb.S['message'] := AErrorMessage;

    InternalRender(LJSonOb.ToJSON, AContext);
  finally
    LJSonOb.Free;
  end;
end;

{ TSwagSecurityDefinitionBasic }

function TSwagSecurityDefinitionBasic.GenerateJsonObject: System.JSON.TJSONObject;
begin
  Result := System.JSON.TJsonObject.Create;
  Result.AddPair('type', ReturnTypeSecurityToString);
  Result.AddPair('description', fDescription);
end;

function TSwagSecurityDefinitionBasic.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := ssdBasic;
end;

end.
