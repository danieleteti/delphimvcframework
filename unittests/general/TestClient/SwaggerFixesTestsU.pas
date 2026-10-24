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
// served by TMVCSwaggerMiddleware (2.0 and 3.x) and by the native OpenAPI()
// emitter. Set DMVC_SWAGGER_DUMP_DIR to keep the documents for an external
// validator (fix_swagger2.json, fix_openapi3.json, fix_native.json).

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
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
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Net.HttpClient,
  MVCFramework.Middleware.Swagger,
  MVCFramework.MinimalAPI,
  MVCFramework.OpenAPI3,
  MVCFramework.Filters,
  MVCFramework.Server.Factory;

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
  fServer.Listen(SWAGFIX_PORT);

  lNativeInfo.Title := 'Native fixes';
  lNativeInfo.Version := 'v1';
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSwagFixController);
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
  lEngine.UseHTTPFilter(OpenAPI(lEngine, lNativeInfo));
  fNative := TMVCServerFactory.CreateIndyDirect(lEngine);
  fNative.Listen(SWAGFIX_NATIVE_PORT);
end;

procedure TSwaggerFixesTests.TeardownFixture;
begin
  fServer.Stop;
  fServer := nil;
  fNative.Stop;
  fNative := nil;
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

initialization

TDUnitX.RegisterTestFixture(TSwaggerFixesTests);

end.
