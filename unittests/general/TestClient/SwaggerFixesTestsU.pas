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
// *************************************************************************** }

unit SwaggerFixesTestsU;

// The schema keyword attributes, a QUERY-only action, an array body in
// Swagger 2.0 and the operationId of a MapMethods route, in the documents
// served by TMVCSwaggerMiddleware (2.0 and 3.x) and by the native OpenAPIHTTPFilter()
// emitter. Set DMVC_SWAGGER_DUMP_DIR to keep the documents for an external
// validator (fix_swagger2.json, fix_openapi3.json, fix_native.json).

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Validators,
  MVCFramework.Server.Intf;

const
  SWAGFIX_PORT = 8926;
  SWAGFIX_NATIVE_PORT = 8927;

type
  [MVCNameCase(ncCamelCase)] // the global default is changed by other fixtures
  TSwagFixDTO = class
  private
    fEmail: string;
    fRank: Integer;
    fScore: Double;
  public
    [MVCRequired]
    [MVCSwagFormat('email')]
    [MVCSwagMaxLength(80)]
    [MVCSwagPattern('^[a-z]+@[a-z]+$')]
    property Email: string read fEmail write fEmail;
    [MVCSwagMinimum(1)]
    [MVCSwagMaximum(10)]
    [MVCSwagFormat('int32')]
    property Rank: Integer read fRank write fRank;
    [MVCSwagMinimum(0)]
    [MVCSwagMaximum(100)]
    [MVCSwagFormat('double')]
    property Score: Double read fScore write fScore;
  end;

  [MVCPath('/api/fix')]
  TSwagFixController = class(TMVCController)
  public
    [MVCPath('/dto')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Fix', 'Get the DTO', 'getFixDTO')]
    [MVCSwagResponses(200, 'Success', TSwagFixDTO)]
    procedure GetDTO;
    [MVCPath('/search')]
    [MVCHTTPMethod([httpQUERY])]
    [MVCSwagSummary('Fix', 'Search', 'searchFix')]
    procedure Search;
    [MVCPath('/batch')]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary('Fix', 'Post many', 'postFixBatch')]
    [MVCSwagParam(plBody, 'items', 'Items', TSwagFixDTO, ptArray)]
    procedure PostBatch;
  end;

  // responses that are not the JSON of a class (native emitter only)
  [MVCPath('/api/bin')]
  TSwagFixBinaryController = class(TMVCController)
  public
    [MVCPath('/image')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('image/png')]
    function CreateImage: TStream;
    [MVCPath('/download')]
    [MVCHTTPMethod([httpGET])]
    function Download: TStream;
    [MVCPath('/thumbnail')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('image/png')]
    [MVCSwagResponses(200, 'The thumbnail')]
    procedure Thumbnail;
    [MVCPath('/wrapped')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagResponses(200, 'Wrapped', TSwagFixDTO, True, True)]
    function Wrapped: IMVCResponse;
  end;

  // many paths, some with QUERY only: the sort of the paths must hold
  [MVCPath('/api/many')]
  TSwagFixManyController = class(TMVCController)
  public
    [MVCPath('/p1')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Many1', 'p1', 'manyP1')]
    procedure P1;
    [MVCPath('/p2')]
    [MVCHTTPMethod([httpQUERY])]
    [MVCSwagSummary('Many2', 'p2', 'manyP2')]
    procedure P2;
    [MVCPath('/p3')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Many0', 'p3', 'manyP3')]
    procedure P3;
    [MVCPath('/p4')]
    [MVCHTTPMethod([httpQUERY])]
    [MVCSwagSummary('Many1', 'p4', 'manyP4')]
    procedure P4;
    [MVCPath('/p5')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Many2', 'p5', 'manyP5')]
    procedure P5;
    [MVCPath('/p6')]
    [MVCHTTPMethod([httpQUERY])]
    [MVCSwagSummary('Many0', 'p6', 'manyP6')]
    procedure P6;
    [MVCPath('/p7')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Many1', 'p7', 'manyP7')]
    procedure P7;
    [MVCPath('/p8')]
    [MVCHTTPMethod([httpQUERY])]
    [MVCSwagSummary('Many2', 'p8', 'manyP8')]
    procedure P8;
  end;

  [TestFixture]
  TSwaggerFixesTests = class
  private
    fServer: IMVCServer;
    fNative: IMVCServer;
    fServerEngine: TMVCEngine; // the servers do not own their engines
    fNativeEngine: TMVCEngine;
    function Fetch(const APort: Integer; const APath, ADumpName: string): TJsonObject;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;
    [Test]
    procedure SchemaKeywordAttributesInSwagger2;
    [Test]
    procedure SchemaKeywordAttributesInOpenAPI3;
    [Test]
    procedure QueryOnlyActionDoesNotBreakSwagger2;
    [Test]
    procedure QueryOnlyActionIsAQueryOperationInOpenAPI3;
    [Test]
    procedure QueryRouteDoesNotBreakTheNativeEmitter;
    [Test]
    procedure Swagger2ArrayBodyHasOnlyASchema;
    [Test]
    procedure WithNameLandsOnTheFirstDocumentedVerb;
    [Test]
    procedure StreamResultIsBinaryOfItsProducesMediaType;
    [Test]
    procedure StreamResultWithoutProducesIsOctetStream;
    [Test]
    procedure SwagResponsesWithoutModelIsBinaryOfItsProducesMediaType;
    [Test]
    procedure SwagResponsesDataEnvelopeWrapsTheModel;
    [Test]
    procedure OpenAPIHTTPFilterAddsTheSourcesItIsGiven;
  end;

implementation

uses
  System.IOUtils,
  System.Net.HttpClient,
  MVCFramework.Middleware.Swagger,
  MVCFramework.MinimalAPI,
  MVCFramework.OpenAPI3,
  MVCFramework.Filters,
  MVCFramework.Filters.OpenAPI,
  MVCFramework.Server.Factory;

type
  TSwagFixExtraSource = class(TInterfacedObject, IMVCOpenAPISource)
  public
    procedure CollectOperations(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
  end;

procedure TSwagFixExtraSource.CollectOperations(const APathsObject: TJsonObject;
  const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
begin
  APathsObject.O['/extra'].O['get'].S['operationId'] := 'extraOperation';
  APathsObject.O['/extra'].O['get'].O['responses'].O['200'].S['description'] := 'OK';
end;

{ TSwagFixBinaryController }

function TSwagFixBinaryController.CreateImage: TStream;
begin
  Result := TMemoryStream.Create;
end;

function TSwagFixBinaryController.Download: TStream;
begin
  Result := TMemoryStream.Create;
end;

procedure TSwagFixBinaryController.Thumbnail;
begin
end;

function TSwagFixBinaryController.Wrapped: IMVCResponse;
begin
  Result := OkResponse;
end;

{ TSwagFixController }

procedure TSwagFixController.GetDTO;
begin
end;

procedure TSwagFixController.Search;
begin
end;

procedure TSwagFixController.PostBatch;
begin
end;

procedure TSwagFixManyController.P1;
begin
end;

procedure TSwagFixManyController.P2;
begin
end;

procedure TSwagFixManyController.P3;
begin
end;

procedure TSwagFixManyController.P4;
begin
end;

procedure TSwagFixManyController.P5;
begin
end;

procedure TSwagFixManyController.P6;
begin
end;

procedure TSwagFixManyController.P7;
begin
end;

procedure TSwagFixManyController.P8;
begin
end;

{ TSwaggerFixesTests }

procedure TSwaggerFixesTests.SetupFixture;
var
  lEngine: TMVCEngine;
  lInfo: TMVCSwaggerInfo;
  lNativeInfo: TMVCOpenAPIInfo;
begin
  lInfo.Title := 'Swagger fixes';
  lInfo.Version := 'v1';
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSwagFixController);
  lEngine.AddController(TSwagFixManyController);
  lEngine.AddMiddleware(TMVCSwaggerMiddleware.Create(lEngine, lInfo, '/swagger.json'));
  lEngine.AddMiddleware(TMVCSwaggerMiddleware.Create(lEngine, lInfo, '/openapi3.json',
    JWT_DEFAULT_DESCRIPTION, False, '', '', '', [psHTTP, psHTTPS], False, ssvOpenAPI3));
  fServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fServerEngine := lEngine;
  fServer.Listen(SWAGFIX_PORT);

  lNativeInfo.Title := 'Native fixes';
  lNativeInfo.Version := 'v1';
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSwagFixController);
  lEngine.AddController(TSwagFixBinaryController);
  lEngine.Root.MapMethods([httpQUERY, httpGET], '/things',
    function: IMVCResponse
    begin
      Result := Ok('things');
    end).WithName('listThings');
  lEngine.Root.MapQuery('/onlyquery',
    function: IMVCResponse
    begin
      Result := Ok('q');
    end).WithName('onlyQuery');
  lEngine.Use(OpenAPIHTTPFilter(lEngine, lNativeInfo, [TSwagFixExtraSource.Create]));
  fNative := TMVCServerFactory.CreateIndyDirect(lEngine);
  fNativeEngine := lEngine;
  fNative.Listen(SWAGFIX_NATIVE_PORT);
end;

procedure TSwaggerFixesTests.TeardownFixture;
begin
  fServer.Stop;
  fServer := nil;
  fNative.Stop;
  fNative := nil;
  FreeAndNil(fServerEngine);
  FreeAndNil(fNativeEngine);
end;

function TSwaggerFixesTests.Fetch(const APort: Integer; const APath, ADumpName: string): TJsonObject;
var
  lClient: THTTPClient;
  lResp: IHTTPResponse;
  lText, lDir: string;
begin
  lClient := THTTPClient.Create;
  try
    lResp := lClient.Get(Format('http://localhost:%d%s', [APort, APath]));
    lText := lResp.ContentAsString(TEncoding.UTF8);
    Assert.AreEqual(200, lResp.StatusCode, APath + ': ' + lText);
  finally
    lClient.Free;
  end;
  lDir := GetEnvironmentVariable('DMVC_SWAGGER_DUMP_DIR');
  if lDir <> '' then
    TFile.WriteAllText(TPath.Combine(lDir, ADumpName), lText, TEncoding.UTF8);
  Result := TJsonObject.Parse(lText) as TJsonObject;
end;

procedure TSwaggerFixesTests.SchemaKeywordAttributesInSwagger2;
var
  lDoc, lProps: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_PORT, '/swagger.json', 'fix_swagger2.json');
  try
    lProps := lDoc.O['definitions'].O['SwagFixDTO'].O['properties'];
    // [MVCRequired] is in the schema's required list, and only that field
    Assert.AreEqual('["email"]', lDoc.O['definitions'].O['SwagFixDTO'].A['required'].ToJSON,
      lDoc.O['definitions'].O['SwagFixDTO'].ToJSON);
    Assert.AreEqual('email', lProps.O['email'].S['format'], lProps.ToJSON);
    Assert.AreEqual(80, lProps.O['email'].I['maxLength'], lProps.ToJSON);
    Assert.AreEqual('^[a-z]+@[a-z]+$', lProps.O['email'].S['pattern'], lProps.ToJSON);
    Assert.AreEqual(1, lProps.O['rank'].I['minimum'], lProps.ToJSON);
    Assert.AreEqual(10, lProps.O['rank'].I['maximum'], lProps.ToJSON);
    Assert.AreEqual('int32', lProps.O['rank'].S['format'], lProps.ToJSON);
    Assert.AreEqual(100, lProps.O['score'].I['maximum'], lProps.ToJSON);
    Assert.AreEqual('double', lProps.O['score'].S['format'], lProps.ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.SchemaKeywordAttributesInOpenAPI3;
var
  lDoc, lProps: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_PORT, '/openapi3.json', 'fix_openapi3.json');
  try
    lProps := lDoc.O['components'].O['schemas'].O['SwagFixDTO'].O['properties'];
    Assert.AreEqual('["email"]', lDoc.O['components'].O['schemas'].O['SwagFixDTO'].A['required'].ToJSON,
      lDoc.O['components'].O['schemas'].O['SwagFixDTO'].ToJSON);
    Assert.AreEqual('email', lProps.O['email'].S['format'], lProps.ToJSON);
    Assert.AreEqual(80, lProps.O['email'].I['maxLength'], lProps.ToJSON);
    Assert.AreEqual('^[a-z]+@[a-z]+$', lProps.O['email'].S['pattern'], lProps.ToJSON);
    Assert.AreEqual(1, lProps.O['rank'].I['minimum'], lProps.ToJSON);
    Assert.AreEqual(10, lProps.O['rank'].I['maximum'], lProps.ToJSON);
    Assert.AreEqual('int32', lProps.O['rank'].S['format'], lProps.ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.QueryOnlyActionDoesNotBreakSwagger2;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_PORT, '/swagger.json', 'fix_swagger2.json');
  try
    Assert.IsTrue(lDoc.O['paths'].Contains('/api/fix/dto'), 'the rest of the document');
    // Swagger 2.0 has no key for QUERY: the operation is left out
    Assert.IsFalse(lDoc.O['paths'].Contains('/api/fix/search') and
      lDoc.O['paths'].O['/api/fix/search'].Contains('query'), lDoc.O['paths'].ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.QueryOnlyActionIsAQueryOperationInOpenAPI3;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_PORT, '/openapi3.json', 'fix_openapi3.json');
  try
    Assert.IsTrue(lDoc.O['paths'].Contains('/api/fix/dto'), 'the rest of the document');
    // OpenAPI 3.2 has the query operation
    Assert.AreEqual('searchFix', lDoc.O['paths'].O['/api/fix/search'].O['query'].S['operationId'],
      lDoc.O['paths'].ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.QueryRouteDoesNotBreakTheNativeEmitter;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.IsTrue(lDoc.O['paths'].Contains('/api/fix/dto'), lDoc.O['paths'].ToJSON);
    Assert.IsTrue(lDoc.O['paths'].Contains('/things'), lDoc.O['paths'].ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.Swagger2ArrayBodyHasOnlyASchema;
var
  lDoc, lParam: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_PORT, '/swagger.json', 'fix_swagger2.json');
  try
    lParam := lDoc.O['paths'].O['/api/fix/batch'].O['post'].A['parameters'].O[0];
    Assert.AreEqual('body', lParam.S['in'], lParam.ToJSON);
    Assert.AreEqual('array', lParam.O['schema'].S['type'], lParam.ToJSON);
    // a body parameter carries its type in the schema only (Swagger 2.0, Parameter Object)
    Assert.IsFalse(lParam.Contains('type'), lParam.ToJSON);
    Assert.IsFalse(lParam.Contains('items'), lParam.ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.WithNameLandsOnTheFirstDocumentedVerb;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.AreEqual('listThings', lDoc.O['paths'].O['/things'].O['get'].S['operationId'],
      lDoc.O['paths'].ToJSON);
  finally
    lDoc.Free;
  end;
end;

function ResponseContent(const ADoc: TJsonObject; const APath, AVerb: string): TJsonObject;
begin
  Result := ADoc.O['paths'].O[APath].O[AVerb].O['responses'].O['200'].O['content'];
end;

procedure AssertNoStreamSchema(const ADoc: TJsonObject);
var
  I: Integer;
begin
  for I := 0 to ADoc.O['components'].O['schemas'].Count - 1 do
    Assert.IsFalse(ADoc.O['components'].O['schemas'].Names[I].Contains('Stream'),
      ADoc.O['components'].O['schemas'].ToJSON);
end;

procedure TSwaggerFixesTests.StreamResultIsBinaryOfItsProducesMediaType;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.AreEqual('{"image/png":{"schema":{"type":"string","format":"binary"}}}',
      ResponseContent(lDoc, '/api/bin/image', 'post').ToJSON);
    AssertNoStreamSchema(lDoc);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.StreamResultWithoutProducesIsOctetStream;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.AreEqual('{"application/octet-stream":{"schema":{"type":"string","format":"binary"}}}',
      ResponseContent(lDoc, '/api/bin/download', 'get').ToJSON);
    AssertNoStreamSchema(lDoc);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.SwagResponsesWithoutModelIsBinaryOfItsProducesMediaType;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.AreEqual('{"image/png":{"schema":{"type":"string","format":"binary"}}}',
      ResponseContent(lDoc, '/api/bin/thumbnail', 'get').ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.SwagResponsesDataEnvelopeWrapsTheModel;
var
  lDoc, lSchema: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    lSchema := ResponseContent(lDoc, '/api/bin/wrapped', 'get').O['application/json'].O['schema'];
    Assert.AreEqual(
      '{"type":"object","properties":{"data":{"type":"array","items":{"$ref":"#/components/schemas/TSwagFixDTO"}}}}',
      lSchema.ToJSON);
  finally
    lDoc.Free;
  end;
end;

procedure TSwaggerFixesTests.OpenAPIHTTPFilterAddsTheSourcesItIsGiven;
var
  lDoc: TJsonObject;
begin
  lDoc := Fetch(SWAGFIX_NATIVE_PORT, '/openapi.json', 'fix_native.json');
  try
    Assert.AreEqual('extraOperation', lDoc.O['paths'].O['/extra'].O['get'].S['operationId'],
      lDoc.O['paths'].ToJSON);
    // the built-in sources are still there
    Assert.IsTrue(lDoc.O['paths'].Contains('/api/fix/dto'), lDoc.O['paths'].ToJSON);
  finally
    lDoc.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TSwaggerFixesTests);

end.
