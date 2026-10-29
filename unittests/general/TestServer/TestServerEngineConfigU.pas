unit TestServerEngineConfigU;

interface

uses
  MVCFramework;

procedure ConfigureTestEngine(AEngine: TMVCEngine);

implementation

uses
  TestServerControllerU,
  TestServerControllerExceptionU,
  SpeedMiddlewareU,
  MVCFramework.Middleware.Authentication,
  MVCFramework.ActiveRecordController,
  System.Generics.Collections,
  MVCFramework.Commons,
  TestServerControllerPrivateU,
  AuthHandlersU,
  TestServerControllerJSONRPCU,
  MinimalAPIWebTestsU,
  EndpointFilterSupportU,
  HTTPFilterSupportU,
  AuthFilterSupportU,
  MVCFramework.OpenAPI3,
  MVCFramework.Middleware.OpenAPI3,
  {$IFNDEF LINUX}
  MVCFramework.View.Renderers.Mustache,
  {$ENDIF}
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.Session,
  MVCFramework.Middleware.StaticFiles
  {$IFDEF MSWINDOWS}
  ,MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes
  {$ENDIF}
  ;

procedure ConfigureTestEngine(AEngine: TMVCEngine);
var
  lOAInfo: TMVCOpenAPIInfo;
begin
  AEngine
    .AddController(TTestServerController)
    .AddController(TTestPrivateServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddController(TTestServerControllerActionFilters)
    .AddController(TTestPrivateServerControllerCustomAuth)
    .AddController(TTestMultiPathController)
    .AddController(TTestActionResultController)
    .AddController(TTestStreamItemsController)
    .AddController(TTestJSONRPCController, '/jsonrpc')
    .AddController(TTestJSONRPCControllerWithGet, '/jsonrpcwithget')
    .AddController(TMVCActiveRecordController, '/api/entities')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCClass.Create
    end, '/jsonrpcclass')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCClassWithGET.Create
    end, '/jsonrpcclasswithget')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClass.Create
    end, '/jsonrpcclass1')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClassWithGet.Create
    end, '/jsonrpcclass1withget')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClassWithGet.Create
    end, '/jsonrpcclass1withget')
    .AddController(TTestFaultController)
    .AddController(TTestFault2Controller,
    function: TMVCController
    begin
      Result := TTestFault2Controller.Create;
    end)
    .AddMiddleware(UseMemorySessionMiddleware())
    .AddMiddleware(TMVCSpeedMiddleware.Create)
    .AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(TCustomAuthHandler.Create, '/system/users/logged'))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www', 'index.html', False))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create('/spa', 'www', 'index.html', True))
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TBasicAuthHandler.Create))
    .AddMiddleware(TMVCCompressionMiddleware.Create);
{$IFDEF MSWINDOWS}
  AEngine.SetViewEngine(TMVCMustacheViewEngine);
  RegisterOptionalCustomTypesSerializers(AEngine.Serializer(TMVCMediaType.APPLICATION_JSON));
{$ENDIF}

  // Minimal-API web extension integration tests
  RegisterMinimalAPIWebRoutes(AEngine);

  // Endpoint filters (post-routing): they act only on their own controller and route and let every
  // other request through, so the whole suite also runs through the endpoint filter chain
  ConfigureEndpointFilterSupport(AEngine);

  // The ready-made HTTP filters, each on a prefix of its own. /fstatic and /fspa serve the folder that the
  // classic middleware serves at /static and /spa, so the tests can compare the two
  ConfigureHTTPFilterSupport(AEngine, 'www');

  // The authentication filters (JWT, JWT cookie, blacklist, OIDC with a fake provider at /idp), each on
  // paths of its own. The suite reaches the TestServer at localhost:8888 on every host
  ConfigureAuthFilterSupport(AEngine, 'http://localhost:8888');

  // OpenAPI 3 endpoint at /openapi.json (used by the Minimal API OpenAPI tests)
  lOAInfo.Title := 'DMVCFramework TestServer';
  lOAInfo.Version := '1.0';
  AEngine.AddMiddleware(TMVCOpenAPI3Middleware.Create(AEngine, lOAInfo));
end;

end.
