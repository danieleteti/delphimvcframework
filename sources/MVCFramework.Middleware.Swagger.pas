unit MVCFramework.Middleware.Swagger;

interface

uses
  MVCFramework,
  Swag.Doc,
  MVCFramework.Swagger.Commons;

type
  TMVCSwaggerMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FEngine: TMVCEngine;
    FSwaggerInfo: TMVCSwaggerInfo;
    FSwagDocURL: string;
    procedure DocumentApiInfo(const ASwagDoc: TSwagDoc);
    procedure DocumentApiSettings(AContext: TWebContext; ASwagDoc: TSwagDoc);
    procedure DocumentApiSecurityDefsJWT(const ASwagDoc: TSwagDoc);
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

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  Swag.Common.Types,
  System.Classes,
  JsonDataObjects,
  System.Rtti,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  MVCFramework.Middleware.JWT,
  System.JSON, Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.SecurityDefinition, Swag.Doc.SecurityDefinitionApiKey;

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
          LSwagPath := TSwagPath.Create;
          LSwagPath.Uri := TMVCSwagger.MVCPathToSwagPath(LControllerPath + LMethodPath);

          for I in LMVCHttpMethods do
          begin
            LSwagPathOp := TSwagPathOperation.Create;
            TMVCSwagger.FillOperationSummary(LSwagPathOp, LMethod);

            LSwagPathOp.Parameters.AddRange(TMVCSwagger.GetParamsFromMethod(LSwagPath.Uri, LMethod));
            LSwagPathOp.Operation := TMVCSwagger.MVCHttpMethodToSwagPathOperation(I);
            LSwagPath.Operations.Add(LSwagPathOp);
          end;
          ASwagDoc.Paths.Add(LSwagPath);
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

procedure TMVCSwaggerMiddleware.DocumentApiSecurityDefsJWT(const ASwagDoc: TSwagDoc);
const
  JWT_SCHEMA =
    '{' + sLineBreak +
    '	"type": "object",' + sLineBreak +
    '	"properties": {' + sLineBreak +
    '		"token": {' + sLineBreak +
    '			"type": "string",' + sLineBreak +
    '			"description": "JWT Token"' + sLineBreak +
    '		}' + sLineBreak +
    '	}' + sLineBreak +
    '}';

var
  LMiddleware: IMVCMiddleware;
  LJWTMiddleware: TMVCJWTAuthenticationMiddleware;
  LRttiContext: TRttiContext;
  LObjType: TRttiType;
  LJwtUrlField: TRttiField;
  LJwtUrlSegment: string;
  LSwagPath: TSwagPath;
  LSwagPathOp: TSwagPathOperation;
  LSwagResponse: TSwagResponse;
  LSwagParam: TSwagRequestParameter;
  LSecurityDefs: TSwagSecurityDefinitionApiKey;
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

        LSwagPath := TSwagPath.Create;
        LSwagPath.Uri := LJwtUrlSegment;

        LSwagPathOp := TSwagPathOperation.Create;
        LSwagPathOp.Tags.Add('JWT Authentication');
        LSwagPathOp.Operation := ohvPost;
        LSwagPathOp.Description := 'Create JSON Web Token';
        LSwagPathOp.Produces.Add(TMVCMediaType.APPLICATION_JSON);

        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := HTTP_STATUS.Unauthorized.ToString;
        LSwagResponse.Description := 'Invalid authorization type';
        LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := HTTP_STATUS.Forbidden.ToString;
        LSwagResponse.Description := 'Forbidden';
        LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := HTTP_STATUS.InternalServerError.ToString;
        LSwagResponse.Description := 'Internal server error';
        LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := HTTP_STATUS.OK.ToString;
        LSwagResponse.Description := 'OK';
        LSwagResponse.Schema.JsonSchema := System.JSON.TJSONObject.ParseJSONValue(JWT_SCHEMA) as System.JSON.TJSONObject;
        LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

        LSwagParam := TSwagRequestParameter.Create;
        LSwagParam.InLocation := rpiHeader;
        LSwagParam.Name := 'Authorization';
        LSwagParam.Required := True;
        LSwagParam.TypeParameter := stpString;
        LSwagParam.Description := 'Contains the word Basic word followed by a space and a base64-encoded ' +
          'string username:password';
        LSwagPathOp.Parameters.Add(LSwagParam);

        LSwagPath.Operations.Add(LSwagPathOp);
        ASwagDoc.Paths.Add(LSwagPath);

        LSecurityDefs := TSwagSecurityDefinitionApiKey.Create;
        LSecurityDefs.SchemaName := 'bearer';
        LSecurityDefs.InLocation := kilHeader;
        LSecurityDefs.Name := 'Authorization';
        LSecurityDefs.Description :=
          'For accessing the API a valid JWT token must be passed in all the queries ' +
          'in the ''Authorization'' header.' + sLineBreak + sLineBreak +
          'A valid JWT token is generated by the API and retourned as answer of a call ' +
          'to the route ' + LJwtUrlSegment + ' giving a valid username and password.' + sLineBreak + sLineBreak +
          'The following syntax must be used in the ''Authorization'' header :' + sLineBreak + sLineBreak +
          '    Bearer xxxxxx.yyyyyyy.zzzzzz' + sLineBreak;

        ASwagDoc.SecurityDefinitions.Add(LSecurityDefs);
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
        DocumentApiSecurityDefsJWT(LSwagDoc);
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

end.
