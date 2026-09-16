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

unit FrameworkTestsU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  BOs,
  MVCFramework, Data.DB, System.SysUtils, MVCFramework.JWT,
  MVCFramework.Serializer.Intf, MVCFramework.Serializer.Defaults,
  MVCFramework.MultiMap, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  MVCFramework.Crypt.Utils, MVCFramework.Filters, MVCFramework.MinimalAPI,
  MVCFramework.Middleware.JWT,
  MVCFramework.Swagger.Commons, Swag.Doc.Path.Operation, Swag.Doc.Definition,
  Swag.Common.Types, Swag.Common.Consts;

type

  [TestFixture]
  TTestRouting = class(TObject)
  private
    FControllers: TObjectList<TMVCControllerDelegate>;
    FMVCActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem>;
    FConfig: TMVCConfig;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestWithNoParameters;
    [Test]
    procedure TestWithNoPath;
    [Test]
    procedure TestPathButNoParameters;
    [Test]
    procedure TestPathWithParameters;
    [Test]
    procedure TestWithMethodTypes;
    [Test]
    procedure TestStringMethodToHTTPMetodKnowsQUERY;
    [Test]
    procedure TestStringMethodToHTTPMetodRejectsUnknownVerbs;
    [Test]
    procedure TestComplexRoutings;
    [Test]
    procedure TestARefusedCandidateLeavesNoParametersBehind;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_338;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_A;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_B;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_C;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_492;
    [Test]
    procedure TestProduceRoutings;
    [Test]
    procedure TestProduceRoutingsWithExplicitCharset;
    [Test]
    procedure TestPathPrefix;
    [Test]
    procedure TestReservedIPs;
  end;

  [TestFixture]
  TTestJWT = class(TObject)
  private
    FJWT: TJWT;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestHMAC;
    [Test]
    procedure TestStorage;
    [Test]
    procedure TestCreateAndValidateToken;
    [Test]
    procedure TestLoadToken;
    [Test]
    procedure TestNotBefore;
    [Test]
    procedure TestExpirationTime;
    [Test]
    procedure TestIssuedAt;
    [Test]
    procedure TestDefaults;
    [Test]
    procedure TheURLAccessTokenParamCanBeTurnedOff;
  end;

  { This is the base test case for all the serunser testcases,
    check 'SerializationFrameworkTestU.pas' }
  [TestFixture]
  TMVCSerUnSerTestCase = class abstract(TObject)
  private
    FSerializer: IMVCSerializer;
  protected
    procedure SetSerializer(const ASerializer: IMVCSerializer);
    [SetUp]
    procedure SetUp;
    function GetObjectsList: TObjectList<TMyObject>;
    function GetObjectsWithStreamsList: TObjectList<TMyStreamObject>;
    function GetObjectsWithTValueList: TObjectList<TMyObjectWithTValue>;
    property Serializer: IMVCSerializer read FSerializer;
    [Test]
    procedure TestSerUnSerObject; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectList; virtual; abstract;
    [Test]
    procedure TestSerUnSerNestedObjects; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectWithStream; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectListWithStream; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectWithTValue; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectListWithTValue; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectStrict; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectBuiltInCustomTypes; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectBuiltInCustomTypesFullObject; virtual; abstract;
  end;

  [TestFixture]
  TTestMultiMap = class(TObject)
  protected
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestObjectMultiMapAdd;
    [Test]
    procedure TestObjectMultiMapRemove;
    [Test]
    procedure TestInterfaceMultiMapAdd;
    [Test]
    procedure TestInterfaceMultiMapRemove;
  end;

  [TestFixture]
  TTestNameCase = class(TObject)
  private
    fOutDATA: array [1 .. 5] of array [ncAsIs .. ncSnakeCase] of string;
    fOrigDATA: array [1 .. 5] of string;
  public
    [SetupFixture]
    procedure SetupFixture;
    [Test]
    procedure TestNameCase;

    [Test]
    [TestCase('LowerCase', 'onetwo,onetwo')]
    [TestCase('LowerCaseWithUnderline', 'one_two,one_two')]
    [TestCase('UpperCase', 'ONETWO,onetwo')]
    [TestCase('UpperCaseWithUnderline', 'ONE_TWO,one_two')]
    [TestCase('PascalCase', 'OneTwo,one_two')]
    [TestCase('CamelCase', 'oneTwo,one_two')]
    [TestCase('UPPERLower1', 'ONETwo,one_two')]
    [TestCase('UPPERLower2', 'OneTWOThree,one_two_three')]
    [TestCase('UPPERLower3', 'DATEOf,date_of')]
    [TestCase('UPPERLower4', 'RESTClient,rest_client')]
    [TestCase('UPPERLowerWithNumber1', 'OneTWO01,one_two_01')]
    [TestCase('UPPERLowerWithNumber2', 'ONE02Three,one_02_three')]
    [TestCase('WithSpaces1', 'One two three,one_two_three')]
    [TestCase('WithSpaces2', 'One  two  three,one_two_three')]
    [TestCase('WithSpaces3', 'One   two   three,one_two_three')]
    [TestCase('WithDots1', 'One.two.three,one_two_three')]
    [TestCase('WithDots2', 'ONE.TWO.THREE,one_two_three')]
    [TestCase('WithDots3', 'ONE.02.THREE,one_02_three')]
    [TestCase('WithUnderlines', 'One_two_three,one_two_three')]
    [TestCase('MultipleUnderlines', 'One___two______three,one_two_three')]
    [TestCase('WithNumber1', 'OneTwo1,one_two_1')]
    [TestCase('WithNumber02', 'OneTwo02,one_two_02')]
    [TestCase('WithNumberInTheMiddle1', 'OneTwo1Two,one_two_1_two')]
    [TestCase('WithNumberInTheMiddle2', 'OneTwo_2two,one_two_2_two')]
    [TestCase('WithNumberInTheMiddle3', 'OneTwo3_Two3,one_two_3_two_3')]
    [TestCase('WithNumberInTheMiddle4', 'OneTwo_4_Two,one_two_4_two')]
    [TestCase('WithNumberInTheMiddle5', 'OneTwo05Three,one_two_05_three')]
    [TestCase('WithNumberInTheMiddle6', 'OneTwo_06Three,one_two_06_three')]
    [TestCase('WithNumberInTheMiddle7', 'OneTwo07_Three,one_two_07_three')]
    [TestCase('WithNumberInTheMiddle8', 'OneTwo_08_Three,one_two_08_three')]
    procedure TestSnakeCase(const AValue1: string; const AValue2: string);

  end;

  [TestFixture]
  TTestCryptUtils = class(TObject)
  public
    [SetupFixture]
    procedure SetupFixture;
    [Test]
    procedure TestPBKDF2_SHA1;
    [Test]
    procedure TestPBKDF2_SHA256;
  end;

  [TestFixture]
  TTestUTC = class(TObject)
  public
    [Test]
    procedure TestStringToDateTime_Local;
    [Test]
    procedure TestStringToDateTime_in_DST_period;
    [Test]
    procedure TestStringToDateTime_in_no_DST_period;
    [Test]
    procedure TestStringToDateTime_NewYork;
    [Test]
    procedure TestStringToDateTime_Mumbai;
    [Test]
    procedure TestDteToStringAndBack;
  end;

  [TestFixture]
  TTestLRUCache = class(TObject)
  public
    [Test]
    [Category('lru')]
    procedure TestPutGet;
    [Test]
    [Category('lru')]
    procedure TestPutGet_Check_No_AV;
  end;


  [TestFixture]
  TTestDotEnv = class(TObject)
  public
    [Test]
    procedure TestWithoutProfiles;
    [Test]
    procedure TestWithDevProfile;
    [Test]
    procedure TestWithDevAndTestProfile;
    [Test]
    procedure TestSkipDefaultWithDevAndTestProfile;
    [Test]
    procedure TestRebuild;
    [Test]
    procedure TestTypedEnv;
    [Test]
    procedure TestRequiredKeys;
  end;

  [TestFixture]
  TTestDotEnvParser = class(TObject)
  public
    [Test]
    procedure TestKeyValue;
    [Test]
    procedure TestWithBadNames;
    [Test]
    procedure TestWithEmptyValue;
    [Test]
    procedure TestKeyValueWithQuotedValues;
    [Test]
    procedure TestValueWithMultiline;
    [Test]
    procedure TestValueWithMultilineEscaped;
    [Test]
    procedure TestVarPlaceHolders;
    [Test]
    procedure TestInLineComments;
    [Test]
    procedure TestErrorLineDetect01;
    [Test]
    procedure TestErrorLineDetect02;
  end;

  [TestFixture]
  TTestSqids = class(TObject)
  public
    [Test]
    procedure TestSingle;
  end;

  [TestFixture]
  TTestRQLCompiler = class(TObject)
  public
    [Test]
    procedure TestFileFixtures;
    [Test]
    procedure Test_MySQL_EscapesBackslash_PreventsSQLInjection;
    [Test]
    procedure UnmappedFieldsCanBeRefused;
  end;

  [TestFixture]
  TTestGenericNullables = class(TObject)
  public
    [Test]
    procedure TestGenericNullables;
  end;

  [TestFixture]
  // The session id reaches the engine from a cookie or a query-string
  // parameter and is then used as a file name by the file store and inside a
  // query by the database store. It must never be anything but the shape
  // GenerateSessionID produces.
  // Security regression fixtures. Each one names the finding it guards so that
  // a future refactor that deletes the guard fails with a message that says
  // what was lost, not just "expected True".
  [TestFixture]
  TTestWizardSecurityDefaults = class(TObject)
  private
    function TemplatesDir: string;
  public
    [Test]
    procedure GeneratedProjectsSetTheJWTCookieSecure;
    [Test]
    procedure GeneratedProjectsSetTheSessionCookieSecure;
  end;

  [TestFixture]
  TTestClientSafeExceptionMessage = class(TObject)
  public
    [Test]
    procedure FrameworkExceptionsKeepTheirMessage;
    [Test]
    procedure ForeignExceptionsAreGenericOutsideDebug;
  end;

  [TestFixture]
  TTestStaticFileWindowsNames = class(TObject)
  public
    [Test]
    procedure AlternateDataStreamIsRefused;
    [Test]
    procedure TrailingDotOrSpaceIsRefused;
    [Test]
    procedure WildcardsAreRefused;
    [Test]
    procedure OrdinaryNamesStillPass;
  end;

  [TestFixture]
  TTestFormFileSaveToFile = class(TObject)
  public
    [Test]
    procedure SaveToFileCannotLeaveTheChosenDirectory;
    [Test]
    procedure SaveToFileRefusesARootedClientName;
    [Test]
    procedure SaveToFileAcceptsARelativeApplicationDirectory;
    [Test]
    procedure SaveToFileKeepsAnOrdinaryName;
  end;

  [TestFixture]
  TTestRateLimitStoreCeiling = class(TObject)
  public
    [Test]
    procedure StoreDoesNotGrowWithoutBound;
  end;

  [TestFixture]
  TTestPathDotSegments = class(TObject)
  public
    [Test]
    procedure DotSegmentsAreDetected;
    [Test]
    procedure DotsInsideNamesAreNotSegments;
  end;

  [TestFixture]
  TTestSessionIDValidation = class(TObject)
  public
    [Test]
    procedure GeneratedIDsAreAccepted;
    [Test]
    procedure TraversalShapesAreRejected;
    [Test]
    procedure RootedAndUNCShapesAreRejected;
    [Test]
    procedure EmptyAndOverlongAreRejected;
    [Test]
    procedure SeparatorsAndDotsAreRejected;
  end;

  TTestStaticFilesTraversal = class(TObject)
  public
    [Test]
    procedure SiblingDirectoryEscapeIsBlocked;
    [Test]
    procedure LegitFileInsideDocRootIsServed;
  end;

  [TestFixture]
  TTestSecurityHelpers = class(TObject)
  public
    [Test]
    procedure MVCStripCRLF_RemovesCRandLF;
    [Test]
    procedure MVCMatchCORSOrigin_ReflectsOnlyMatchingOrigin;
    [Test]
    procedure MVCCORSAllowsCredentials_NeverOnAWildcardOrigin;
    [Test]
    procedure TMVCFormFile_SafeFileName_StripsPathComponents;
    [Test]
    procedure MVCResolveClientIP_IgnoresForwardedHeadersUnlessTrusted;
    [Test]
    procedure MVCRedactSecret_MasksCredentialKeepsScheme;
  end;


  {An action declared only in the base class - the shape of TMVCSSEController.EventStream}
  TSwagBaseController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure InheritedAction;
    [MVCPath('/described')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Streams', 'Streams the events', 'streamEvents')]
    procedure DescribedAction;
    [MVCPath('/multiverb')]
    [MVCHTTPMethod([httpGET])]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    procedure MultiVerbAction;
    [MVCPath('/noverb')]
    procedure NoVerbAction;
  end;

  [MVCSWAGDefaultSummaryTags('Events')]
  TSwagDerivedController = class(TSwagBaseController)
  end;

  [TestFixture]
  TTestSwaggerMetadata = class(TObject)
  private
    function OperationFor(const AMethodName: string;
      const ADefaultTags: TArray<String>): TSwagPathOperation;
  public
    [Test]
    procedure InheritedActionUsesTheControllerDefaultTags;
    [Test]
    procedure InheritedActionWithoutDefaultTagsKeepsTheOldFallback;
    [Test]
    procedure SummaryIsFilledNotOnlyDescription;
    [Test]
    procedure AllowedMethodsUnionsEveryAttribute;
    [Test]
    procedure AllowedMethodsDefaultsToEveryVerb;
    [Test]
    procedure VerbsOpenAPI2CannotExpressAreSkipped;
  end;

  // Guards the JsonMaxNestingDepth patch carried on top of the vendored
  // JsonDataObjects.pas. Upstream (ahausladen/JsonDataObjects) has no nesting
  // limit: its recursive-descent parser spends one stack frame per '{' or '[',
  // so ~8500 levels -- 34 KB of perfectly valid JSON, well under the 5 MiB
  // DEFAULT_MAX_REQUEST_SIZE -- kill a Win64 process outright, with no
  // exception to catch. If a future re-sync with upstream drops the patch this
  // fixture must fail, and it is written to fail loudly: merely referencing
  // JsonMaxNestingDepth means the unit stops compiling without it.
  [TestFixture]
  TTestJSONNestingDepth = class(TObject)
  private
    function NestedJSON(const ADepth: Integer; const AArrays: Boolean): string;
  public
    [Test]
    procedure TheLimitIsInPlaceAndSane;
    [Test]
    procedure AtTheLimitItParses;
    [Test]
    procedure PastTheLimitItRaisesInsteadOfCrashing;
    [Test]
    procedure StrToJSONObjectSurvivesADeeplyNestedBody;
    [Test]
    procedure TheDepthCounterDoesNotLeakAcrossParses;
    [Test]
    procedure ClearingTheLimitDoesNotDisableIt;
  end;

implementation

{$WARN SYMBOL_DEPRECATED OFF}

uses
  System.DateUtils, System.TimeSpan, System.Math,
  TestControllersU, DBClient,
  Web.HTTPApp, Soap.EncdDecd,
  IdHashMessageDigest, idHash,
  System.Threading,
  MVCFramework.RQL.Parser,
  MVCFramework.HMAC, System.Diagnostics,
  MVCFramework.LRUCache,

{$IF CompilerVersion < 27}
  Data.DBXJSON,

{$ELSE}
  System.JSON,

{$ENDIF}
  TestServerControllerU, System.Classes,
  MVCFramework.DuckTyping, System.IOUtils, MVCFramework.SystemJSONUtils,
  IdGlobal, System.TypInfo, System.Types, Winapi.Windows, MVCFramework.DotEnv,
  MVCFramework.DotEnv.Parser, MVCFramework.Nullables, System.Rtti,
  MVCFramework.Session, MVCFramework.Middleware.RateLimit, JsonDataObjects;

var
  JWT_SECRET_KEY_TEST: string = 'myk3y';
  HMAC_ALG_AND_RESULTS: array [0 .. 4] of array [0 .. 1] of string = (
    (
      'md5',
      '5256311089fa9c80f735fb8cc28bf4fe'
    ),
    (
      'sha1',
      '323ff5f4e53c43f2d9342952299a9d35f9ee5dc2'
    ),
    (
      'sha224',
      '2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35'
    ),
    (
      'sha256',
      '1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b'
    ),
    (
      'sha512',
      '22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9'
    )
  );

function MD5(const aStream: TStream): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  aStream.Position := 0;
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashBytesAsHex(idmd5.HashStream(aStream));
  finally
    idmd5.Free;
  end;
end;

procedure TTestRouting.SetUp;
begin
  FControllers := TObjectList<TMVCControllerDelegate>.Create;
  FControllers.Add(TMVCControllerDelegate.Create(TSimpleController, nil));
  FControllers.Add(TMVCControllerDelegate.Create(TNotSoSimpleController, nil));
  FControllers.Add(TMVCControllerDelegate.Create(TTestServerController, nil));
  FMVCActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
  FConfig := TMVCConfig.Create;
  FConfig.Value[TMVCConfigKey.PathPrefix] := '';
end;

procedure TTestRouting.TearDown;
begin
  FControllers.Free;
  FMVCActionParamsCache.Free;
  FConfig.Free;
end;

procedure TTestRouting.TestStringMethodToHTTPMetodKnowsQUERY;
begin
  Assert.IsTrue(TMVCRouter.StringMethodToHTTPMetod('QUERY') = httpQUERY);
end;

procedure TTestRouting.TestStringMethodToHTTPMetodRejectsUnknownVerbs;
begin
  {Guardrail: an unmapped verb must still fail loudly. A permissive fallback
   here would silently dispatch it as some other verb.}
  Assert.WillRaise(
    procedure
    begin
      TMVCRouter.StringMethodToHTTPMetod('FROB');
    end, EMVCException);
end;

procedure TTestRouting.TestARefusedCandidateLeavesNoParametersBehind;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
  lMatched: Boolean;
begin
  { A route parameter whose DECODED value carries a dot segment makes the router
    refuse the candidate - the regex matched one segment and the action would
    have received three. The refusal happens in the middle of the loop that fills
    the parameters table, and that table is shared by every candidate: two GET
    actions on the same path (content negotiation) mean the second candidate then
    calls Add on a key the first one left behind, and TMVCRequestParamsTable is a
    TDictionary, so that raises EListError - a 500 where the request should
    simply not match. The refusing candidate has to put the table back. }
  Params := TMVCRequestParamsTable.Create;
  try
    lMatched := True;
    Assert.WillNotRaise(
      procedure
      begin
        lMatched := TMVCRouter.ExecuteRouting('/negotiated/docs/%2E%2E', httpGET,
          'text/plain', 'application/json', FControllers, 'text/plain',
          TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult);
      end, EListError,
      'the second candidate must not trip over the parameters the first one left');
    Assert.isFalse(lMatched, 'a dot segment inside a path parameter must not match');
    Assert.IsTrue(Params.Count = 0,
      'a candidate that refused the path must leave the parameters table as it found it, ' +
      'but it left ' + Params.Count.ToString + ' behind');
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestComplexRoutings;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/path1/1', httpPOST, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('TestMultiplePaths', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('TestMultiplePaths', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('TestMultiplePaths', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('TestMultiplePaths', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(TMVCRouter.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_338;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/338
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/issue338/projectid/pictures/imageuuid', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.AreEqual('GetImage', lRouterResult.MethodToCall.Name);
    Assert.AreEqual<Integer>(2, Params.Count);
    Assert.AreEqual('projectid', Params['projectid']);
    Assert.AreEqual('imageuuid', Params['imageuuid']);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/issue338/projectid', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('GetProject', lRouterResult.MethodToCall.Name);
    Assert.areEqual<Integer>(1, Params.Count);
    Assert.areEqual('projectid', Params['projectid']);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_492;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/492
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/issue492/delphi$mvc$framework', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('GetIssue492', lRouterResult.MethodToCall.Name);
    Assert.areEqual<Integer>(1, Params.Count);
    Assert.areEqual('delphi$mvc$framework', Params['stringvalue']);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_513_A;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/patient/$match', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('GetOrderIssue513', lRouterResult.MethodToCall.Name);
    Assert.areEqual<Integer>(0, Params.Count);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.Test_ISSUE_513_B;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/patient/$match/daniele/teti', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('GetOrderIssue513WithPars', lRouterResult.MethodToCall.Name);
    Assert.areEqual<Integer>(2, Params.Count);
    Assert.areEqual('daniele', Params['par1']);
    Assert.areEqual('teti', Params['par2']);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.Test_ISSUE_513_C;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/patient/$match/da$niele/te$ti', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('GetOrderIssue513WithPars', lRouterResult.MethodToCall.Name);
    Assert.areEqual<Integer>(2, Params.Count);
    Assert.areEqual('da$niele', Params['par1']);
    Assert.areEqual('te$ti', Params['par2']);
  finally
    Params.Free;
  end;

end;


// procedure TTestMappers.TestDataSetToJSONArray;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// JArr: TJSONArray;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// ds.First;
// // JArr := TJSONArray.Create;
// JArr := ds.AsJSONArray;
// try
// // Mapper.DataSetToJSONArray(ds, JArr, false);
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds.First;
// while not ds.Eof do
// begin
// ds2.Insert;
// JObj := JArr.Get(ds.RecNo - 1) as TJSONObject;
// ds2.LoadFromJSONObject(JObj);
// // Mapper.JSONObjectToDataSet(JObj, ds2, false);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// ds.Next;
// end;
// finally
// JArr.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObject;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject;
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyAsIsCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpAsIs);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpAsIs);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyLowerCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpLowerCase);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpLowerCase);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;
//
// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyUpperCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpUpperCase);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpUpperCase);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectWithNulls;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// begin
// ds := TClientDataSet.Create(nil);
// try
// ds.FieldDefs.Add('string_value', ftString, 50);
// ds.FieldDefs.Add('integer_value', ftInteger);
// ds.FieldDefs.Add('float_value', ftFloat);
// ds.FieldDefs.Add('null_value', ftString, 50);
// ds.FieldDefs.Add('boolean_value', ftBoolean);
// ds.CreateDataSet;
// ds.Insert;
// ds.FieldByName('string_value').AsString := 'myStringValue';
// ds.FieldByName('integer_value').AsInteger := 123;
// ds.FieldByName('float_value').AsFloat := 123.456;
// ds.FieldByName('null_value').Clear;
// ds.FieldByName('boolean_value').AsBoolean := true;
// ds.Post;
// JObj := ds.AsJSONObject;
// try
// Assert.areEqual('myStringValue', JObj.Values['string_value'].Value);
// Assert.areEqual(123, JObj.Values['integer_value'].GetValue<TJSONNumber>().AsInt);
// Assert.areEqual(123.456, JObj.Values['float_value'].GetValue<TJSONNumber>().AsDouble, 0.0009);
// Assert.isTrue(JObj.Values['null_value'].GetValue<TJSONNull>().Null);
// Assert.areEqual(true, JObj.Values['boolean_value'].GetValue<TJSONBool>().AsBoolean);
// Assert.isTrue(JObj.ToJSON.Replace(' ', '').Contains('"null_value":null'));
// ds.Insert;
// ds.LoadFromJSONObject(JObj);
// ds.Post;
// Assert.isTrue(ds.FieldByName('null_value').IsNull);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// end;
// end;

// procedure TTestMappers.TestJSONArrayToObjectListNoGenerics;
// var
// ListObj, RetList: TObjectList<TMyObject>;
// JSONArr: TJSONArray;
// I: Integer;
// begin
// ListObj := TObjectList<TMyObject>.Create;
// try
// ListObj.Add(GetMyObject);
// ListObj.Add(GetMyObject);
// JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
// try
// RetList := TObjectList<TMyObject>(Mapper.JSONArrayToObjectList(TMyObject,
// JSONArr, false));
// try
// Assert.areEqual(2, RetList.Count);
// for I := 0 to ListObj.Count - 1 do
// Assert.isTrue(ListObj[I].Equals(RetList[I]));
// finally
// RetList.Free;
// end;
// finally
// JSONArr.Free;
// end;
// finally
// ListObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONArrayToObjectListNoGenericsWrappedList;
// var
// ListObj, RetList: TObjectList<TMyObject>;
// JSONArr: TJSONArray;
// I: Integer;
// begin
// ListObj := TObjectList<TMyObject>.Create;
// try
// ListObj.Add(GetMyObject);
// ListObj.Add(GetMyObject);
// JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
// try
// RetList := TObjectList<TMyObject>.Create;
// try
// Mapper.JSONArrayToObjectList(WrapAsList(RetList), TMyObject,
// JSONArr, false);
// Assert.areEqual(2, RetList.Count);
// for I := 0 to ListObj.Count - 1 do
// Assert.isTrue(ListObj[I].Equals(RetList[I]));
// finally
// RetList.Free;
// end;
// finally
// JSONArr.Free;
// end;
// finally
// ListObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectStringToObject;
// const
// MYOBJECTJSON =
// '{"PropString":"Some text \u00E0\u00E8\u00E9\u00EC\u00F2\u00F9",' +
// '"PropAnsiString":"This is an ANSI text","PropInteger":-1234,' +
// '"PropUInt32":1234,"PropInt64":-1234567890,"PropUInt64":1234567890,' +
// '"PropUInt16":12345,"PropInt16":-12345,"PropBoolean":true,' +
// '"PropDate":"2010-10-20","PropTime":"10:20:30",' +
// '"PropDateTime":"2010-10-20 10:20:30",' +
// '"PropTimeStamp":63423339630040,"PropCurrency":1234.5678}';
// var
// lMyObject: TMyObject;
// lMyObject2: TMyObject;
// begin
// lMyObject := Mapper.JSONObjectStringToObject<TMyObject>(MYOBJECTJSON);
// try
// lMyObject2 := GetMyObject;
// try
// Assert.isTrue(lMyObject.Equals(lMyObject2));
// finally
// lMyObject2.Free;
// end;
// finally
// lMyObject.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectStringToObjectWithWrongJSON;
// begin
// ExpectedException := EMapperException;
// Mapper.JSONObjectStringToObject<TObject>('{wrongjson}');
// end;
//
// procedure TTestMappers.TestJSONObjectToObjectAndBack;
// var
// Obj: TMyObject;
// JObj: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyObject>(JObj);
// try
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JObj.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectToObjectWithNullInJSONString;
// var
// LJSONObject: string;
// Obj: TMyStreamObject;
// begin
// LJSONObject := '{"ImageStream":null}';
// Obj := Mapper.JSONObjectStringToObject<TMyStreamObject>(LJSONObject);
// Assert.isNull(Obj.ImageStream);
// Obj.Free;
// end;
//
// procedure TTestMappers.TestLoadJSONObjectToObjectAndBack;
// var
// Obj: TMyObject;
// JObj: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := TMyObject.Create;
// try
// Mapper.LoadJSONObjectToObject<TMyObject>(JObj, Obj2);
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JObj.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectListToJSONArray;
// var
// Obj: TMyObject;
// ObjList, Obj2List: TObjectList<TMyObject>;
// JSON: TJSONArray;
// I: Integer;
// begin
// ObjList := TObjectList<TMyObject>.Create(true);
// try
// for I := 1 to 10 do
// begin
// Obj := GetMyObject;
// Obj.PropInteger := I;
// ObjList.Add(Obj);
// end;
// JSON := Mapper.ObjectListToJSONArray<TMyObject>(ObjList);
//
// Obj2List := Mapper.JSONArrayToObjectList<TMyObject>(JSON);
// try
// Assert.areEqual(ObjList.Count, Obj2List.Count);
// for I := 0 to 9 do
// begin
// Assert.isTrue(Obj2List[I].Equals(ObjList[I]));
// end;
// finally
// Obj2List.Free;
// end;
// finally
// ObjList.Free;
// end;
// end;
//
// procedure TTestMappers.TestWrappedListToJSONArray;
// var
// Obj: TMyObject;
// ObjList: TObjectList<TMyObject>;
// WrapList: IWrappedList;
// JSON: TJSONArray;
// I: Integer;
// LJSONObj: TJSONObject;
// LMyItem: TMyObject;
// begin
// ObjList := TObjectList<TMyObject>.Create(true);
// try
// for I := 1 to 10 do
// begin
// Obj := GetMyObject;
// Obj.PropInteger := I;
// ObjList.Add(Obj);
// end;
// WrapList := WrapAsList(ObjList);
// JSON := Mapper.ObjectListToJSONArray(WrapList);
// try
// Assert.areEqual(WrapList.Count, JSON.Count);
// for I := 0 to 9 do
// begin
// LJSONObj := JSON.Items[I] as TJSONObject;
// LMyItem := WrapList.GetItem(I) as TMyObject;
// Assert.areEqual(LMyItem.PropInteger, LJSONObj.GetValue<Integer>('PropInteger'));
// end;
// finally
// JSON.Free;
// end;
// finally
// ObjList.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObject;
// var
// Obj: TMyObject;
// JSON: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JSON := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyObject>(JSON);
// try
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JSON.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStream;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// begin
// // ARRANGE
// SO := TMyStreamObject.Create;
// try
// // ACT
// TMemoryStream(SO.ImageStream)
// .LoadFromFile('..\..\..\..\..\samples\_\customer.png');
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// // ASSERT
// Assert.areEqual(SO.ImageStream.Size, ResultSO.ImageStream.Size);
// Assert.areEqual(MD5(SO.ImageStream), MD5(ResultSO.ImageStream));
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF16;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// ResultStr, str: UnicodeString;
// begin
// // ARRANGE
// str := 'This is a UTF16 String (什么是)';
// SO := TMyStreamObject.Create;
// try
// // ACT
// SO.PropStream := TStringStream.Create(str, TEncoding.Unicode);
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// ResultStr := TStringStream(ResultSO.PropStream).DataString;
// // ASSERT
// Assert.areEqual(str, ResultStr);
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF8;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// ResultStr, str: UTF8String;
// begin
// // ARRANGE
// str := 'This is a UTF8 String (什么是)';
// SO := TMyStreamObject.Create;
// try
// // ACT
// SO.Prop8Stream := TStringStream.Create(string(str), TEncoding.UTF8);
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// ResultStr := UTF8String(TStringStream(ResultSO.Prop8Stream).DataString);
// // ASSERT
// Assert.areEqual(str, ResultStr);
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObject_Generics;
// var
// lObjList: TObjectList<TMyClass>;
// lResponse: TResponseWrapper<TMyClass>;
// LJSONObj: TJSONObject;
// begin
// lObjList := TObjectList<TMyClass>.Create();
// lObjList.Add(TMyClass.Create(1, 'pippo'));
// lObjList.Add(TMyClass.Create(2, 'pluto'));
// lResponse := TResponseWrapper<TMyClass>.Create(lObjList.Count, lObjList);
// try
// LJSONObj := Mapper.ObjectToJSONObject(lResponse);
// try
// CheckNotNull(LJSONObj.GetValue('Items'));
// Assert.areEqual(2, TJSONArray(LJSONObj.GetValue('Items')).Count);
// finally
// LJSONObj.Free;
// end;
// finally
// lResponse.Free;
// end;
// end;

procedure TTestRouting.TestPathButNoParameters;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, lRouterResult));
    Assert.areEqual<Integer>(0, Params.Count);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('Orders', lRouterResult.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, lRouterResult.ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestPathPrefix;
var
  lControllers: TObjectList<TMVCControllerDelegate>;
  lMVCActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem>;
  lConfig: TMVCConfig;
  lParams: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  lControllers := TObjectList<TMVCControllerDelegate>.Create;
  lControllers.Add(TMVCControllerDelegate.Create(TSimpleController, nil));
  lControllers.Add(TMVCControllerDelegate.Create(TNotSoSimpleController, nil));
  lControllers.Add(TMVCControllerDelegate.Create(TTestServerController, nil));
  lMVCActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
  lConfig := TMVCConfig.Create;
  lConfig.Value[TMVCConfigKey.PathPrefix] := '';
  try
    lParams := TMVCRequestParamsTable.Create;
    try
      Assert.isFalse(TMVCRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain', 'text/plain', FControllers,
        'text/plain', TMVCConstants.DEFAULT_CONTENT_CHARSET, '', lParams, lRouterResult));

      Assert.isTrue(TMVCRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain', 'text/plain', FControllers,
        'text/plain', TMVCConstants.DEFAULT_CONTENT_CHARSET, '/api', lParams, lRouterResult));
      Assert.areEqual<Integer>(0, lParams.Count);
      Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
      Assert.areEqual('Orders', lRouterResult.MethodToCall.Name);
      Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, lRouterResult.ResponseContentCharset);
    finally
      lParams.Free;
    end;

  finally
    lControllers.Free;
    lMVCActionParamsCache.Free;
    lConfig.Free;
  end;
end;

procedure TTestRouting.TestPathWithParameters;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual<Integer>(1, Params.Count);
    Assert.areEqual('789', Params['ordernumber']);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('OrderNumber', lRouterResult.MethodToCall.Name);
  finally
    Params.Free;
  end;

  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/àèéìòù .-_\', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual<Integer>(1, Params.Count);
    Assert.areEqual('àèéìòù .-_\', Params['ordernumber']);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('OrderNumber', lRouterResult.MethodToCall.Name);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.TestProduceRoutings;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    // a GET request with a ACCEPT: application/json
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders', httpGET, '', 'application/json', FControllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, lRouterResult));
    Assert.areEqual<Integer>(0, Params.Count);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('OrdersProduceJSON', lRouterResult.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, lRouterResult.ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestProduceRoutingsWithExplicitCharset;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    // a GET request with a ACCEPT: application/json
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders', httpGET, '', 'application/json; charset=UTF-8', FControllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, lRouterResult));
    Assert.areEqual<Integer>(0, Params.Count);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('OrdersProduceJSON', lRouterResult.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, lRouterResult.ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestReservedIPs;
var
  I: Integer;
begin
  // this test just tests the IP2Long implementation
  for I := low(RESERVED_IPv4) to high(RESERVED_IPv4) do
  begin
    Assert.areEqual(IPv4ToUInt32(RESERVED_IPv4[I][1]), IP2Long(RESERVED_IPv4[I][1]));
    Assert.areEqual(IPv4ToUInt32(RESERVED_IPv4[I][2]), IP2Long(RESERVED_IPv4[I][2]));
  end;
end;

{ Use this test only if you want to test the speed of the router }
// procedure TTestRouting.TestRoutingSpeed;
// var
// Params: TMVCRequestParamsTable;
// ResponseContentType: string;
// ResponseContentEncoding: string;
// I: Integer;
// lSW: TStopwatch;
// begin
// // procedure TestTypedActionBooleans(bool1, bool2, bool3, bool4: Boolean);
// Params := TMVCRequestParamsTable.Create;
// try
// lSW := TStopWatch.Create;
// lSW.Start;
// for I := 1 to 1000 do
// begin
// Params.Clear;
// Router.ExecuteRouting(
// '/typed/booleans/true/false/true/false',
// httpGET,
// TMVCMediaType.APPLICATION_JSON,
// TMVCMediaType.APPLICATION_JSON,
// Controllers,
// TMVCMediaType.APPLICATION_JSON,
// TMVCMediaType.APPLICATION_JSON,
// Params,
// ResponseContentType, ResponseContentEncoding, lRouterResult);
// end;
// Assert.isTrue(false, lSW.ElapsedMilliseconds.ToString);
// finally
// Params.Free;
// end;
//
// end;

// procedure TTestMappers.TestSerializeUsingFields;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TObject;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(4, lJObj.Count); // 3 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FFirstName'));
// CheckNotNull(lJObj.Get('FLastName'));
// CheckNotNull(lJObj.Get('FAge'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyObjectWithLogic,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingFieldsComplexObject;
// var
// lJObj: TJSONObject;
// lObj2: TObject;
// lObj: TMyComplexObject;
// begin
// lObj := GetMyComplexObject;
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 4 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FProp1'));
// CheckNotNull(lJObj.Get('FChildObjectList'));
// CheckNotNull(lJObj.Get('FChildObject'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyComplexObject,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingFieldsComplexObject2;
// var
// lJObj: TJSONObject;
// lObj2: TObject;
// lObj: TMyComplexObject;
// begin
// lObj := GetMyComplexObjectWithNotInitializedChilds;
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 4 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FProp1'));
// CheckNotNull(lJObj.Get('FChildObjectList'));
// CheckNotNull(lJObj.Get('FChildObject'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyComplexObject,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.
// TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TMyObjectWithLogic;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// lJObj.RemovePair('FFirstName').Free;
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj) as TMyObjectWithLogic;
// try
// Assert.areEqual('', lObj2.FirstName);
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingProperties;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TMyObjectWithLogic;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObject(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 5 properties
// CheckNotNull(lJObj.Get('FirstName'));
// CheckNotNull(lJObj.Get('LastName'));
// CheckNotNull(lJObj.Get('Age'));
// CheckNotNull(lJObj.Get('FullName'));
// CheckNotNull(lJObj.Get('IsAdult'));
// lObj2 := Mapper.JSONObjectToObject<TMyObjectWithLogic>(lJObj);
// try
// Assert.isTrue(lObj2.Equals(lObj),
// 'deserialized object is not equals to the original object');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;

procedure TTestRouting.TestWithMethodTypes;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('UpdateOrderNumber', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpPUT, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('UpdateOrderNumber', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpPATCH, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('PatchOrder', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(TMVCRouter.ExecuteRouting('/orders/789', httpDELETE, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));

    Params.Clear;
    Assert.isFalse(TMVCRouter.ExecuteRouting('/orders/789', httpHEAD, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult),
      'Resolved as HEAD');

    Params.Clear;
    Assert.isFalse(TMVCRouter.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult),
      'Resolved as OPTIONS');

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('OrderNumber', lRouterResult.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(TMVCRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual('OrderNumber', lRouterResult.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestWithNoParameters;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual<Integer>(0, Params.Count);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('Index', lRouterResult.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestWithNoPath;
var
  Params: TMVCRequestParamsTable;
  lRouterResult: TMVCRouterResult;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(TMVCRouter.ExecuteRouting('', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, lRouterResult));
    Assert.areEqual<Integer>(0, Params.Count);
    Assert.areEqual('TSimpleController', lRouterResult.ControllerClazz.ClassName);
    Assert.areEqual('Index', lRouterResult.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

{ TTestJWT }

type
  // Minimal handler: the middleware needs one to be built, these tests never
  // reach an authentication decision.
  TNullAuthenticationHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName: string;
      const APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TSessionData);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

procedure TNullAuthenticationHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True;
end;

procedure TNullAuthenticationHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName: string; const APassword: string; AUserRoles: TList<string>;
  var AIsValid: Boolean; const ASessionData: TSessionData);
begin
  AIsValid := False;
end;

procedure TNullAuthenticationHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
  const AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := False;
end;

procedure TTestJWT.TheURLAccessTokenParamCanBeTurnedOff;
var
  lMiddleware: TMVCJWTAuthenticationMiddleware;
  lMiddlewareRef: IMVCMiddleware;
begin
  { A token in the URL ends up in proxy logs, browser history and Referer headers.
    It is still read by default - SSE, <img> and download links have no way to send
    a header - but an API that does not need it must be able to say so, and until
    now the parameter name was a private field nobody could reach. }
  lMiddleware := TMVCJWTAuthenticationMiddleware.Create(
    TNullAuthenticationHandler.Create, nil, 'a-secret-long-enough-to-be-accepted-here');
  lMiddlewareRef := lMiddleware;
  Assert.AreEqual('access_token', lMiddleware.AuthorizationAccessTokenParamName,
    'the 3.4 default must survive the upgrade');
  lMiddleware.AuthorizationAccessTokenParamName := '';
  Assert.AreEqual('', lMiddleware.AuthorizationAccessTokenParamName,
    'an empty name means: only the Authorization header is accepted');
end;

procedure TTestJWT.SetUp;
begin
  inherited;
  FJWT := TJWT.Create(JWT_SECRET_KEY_TEST);
end;

procedure TTestJWT.TearDown;
begin
  FJWT.Free;
  inherited;
end;

procedure TTestJWT.TestCreateAndValidateToken;
var
  lToken: string;
  lError: string;
begin
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.JWT_ID := TGUID.NewGuid.ToString;
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';
  FJWT.Claims.ExpirationTime := Tomorrow;
  FJWT.Claims.IssuedAt := Yesterday;
  FJWT.Claims.NotBefore := Yesterday;
  lToken := FJWT.GetToken;
  // TFile.WriteAllText('jwt_token.dat', lToken);

  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Generated token is not valid');
end;

procedure TTestJWT.TestDefaults;
begin
  Assert.areEqual('HS512', FJWT.HMACAlgorithm, 'Default algorithm should be HS512');
  Assert.areEqual(300, FJWT.LeewaySeconds, 'Default leeway should be 5 minutes');
  if FJWT.RegClaimsToChecks * [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
    TJWTCheckableClaim.IssuedAt] <> [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
    TJWTCheckableClaim.IssuedAt] then
    Assert.Fail('Default RegClaimsToCheck not correct');
end;

procedure TTestJWT.TestExpirationTime;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime];
  FJWT.Claims.ExpirationTime := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Expired token is considered valid');

  FJWT.Claims.ExpirationTime := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Now - (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Expired token is considered valid');
end;

procedure TTestJWT.TestHMAC;
var
  lAlg: string;
  lValue: string;
  I: Integer;
begin
  for I := low(HMAC_ALG_AND_RESULTS) to high(HMAC_ALG_AND_RESULTS) do
  begin
    lAlg := HMAC_ALG_AND_RESULTS[I][0];
    lValue := HMAC_ALG_AND_RESULTS[I][1];
    Assert.areEqual(lValue, BytesToHex(HMAC(lAlg, 'Daniele Teti', 'daniele')), 'HMAC ' + lAlg + ' fails');
  end;
end;

procedure TTestJWT.TestIssuedAt;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.IssuedAt];
  FJWT.Claims.IssuedAt := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');

  FJWT.Claims.IssuedAt := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestLoadToken;
var
  lToken: string;
  lJWT: TJWT;
  lError: string;
  lExp: TDateTime;
begin
  lExp := Now + OneHour * 2;
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.Audience := 'DelphiDevelopers';
  FJWT.Claims.IssuedAt := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.ExpirationTime := lExp;
  FJWT.Claims.NotBefore := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.JWT_ID := '123456';
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';

  lToken := FJWT.GetToken;
  // TFile.WriteAllText('jwt_token_full.dat', lToken);

  lJWT := TJWT.Create(JWT_SECRET_KEY_TEST);
  try
    lJWT.LoadToken(lToken, lError);
    Assert.areEqual('bit Time Professionals', lJWT.Claims.Issuer);
    Assert.areEqual('DelphiMVCFramework', lJWT.Claims.Subject);
    Assert.areEqual('DelphiDevelopers', lJWT.Claims.Audience);
    Assert.areEqual('123456', lJWT.Claims.JWT_ID);
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), lJWT.Claims.IssuedAt);
    Assert.areEqual(Roundto(lExp, 4), Roundto(lJWT.Claims.ExpirationTime, 4));
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), lJWT.Claims.NotBefore);
    Assert.areEqual('dteti', lJWT.CustomClaims['username']);
    Assert.areEqual('admin', lJWT.CustomClaims['userrole']);
  finally
    lJWT.Free;
  end;

end;

procedure TTestJWT.TestNotBefore;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.NotBefore];
  FJWT.Claims.NotBefore := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid (near midnight is ok... fix this test) ');

  FJWT.Claims.NotBefore := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestStorage;
begin
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.Audience := 'DelphiDevelopers';
  FJWT.Claims.IssuedAt := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.ExpirationTime := FJWT.Claims.IssuedAt + OneHour * 2;
  FJWT.Claims.NotBefore := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.JWT_ID := '123456';
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';

  Assert.areEqual('bit Time Professionals', FJWT.Claims.Issuer);
  Assert.areEqual('DelphiMVCFramework', FJWT.Claims.Subject);
  Assert.areEqual('DelphiDevelopers', FJWT.Claims.Audience);
  Assert.areEqual('123456', FJWT.Claims.JWT_ID);
  Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), FJWT.Claims.IssuedAt);
  Assert.areEqual(Roundto(FJWT.Claims.IssuedAt + OneHour * 2, 4), Roundto(FJWT.Claims.ExpirationTime, 4));
  Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), FJWT.Claims.NotBefore);

  Assert.areEqual('dteti', FJWT.CustomClaims['username']);
  Assert.areEqual('admin', FJWT.CustomClaims['userrole']);

end;

{ TMVCSerUnSerTestCase }

function TMVCSerUnSerTestCase.GetObjectsList: TObjectList<TMyObject>;
var
  I: Integer;
begin
  Result := TObjectList<TMyObject>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObject);
    Result.Last.PropInteger := I;
  end;
end;

function TMVCSerUnSerTestCase.GetObjectsWithTValueList: TObjectList<TMyObjectWithTValue>;
var
  I: Integer;
begin
  Result := TObjectList<TMyObjectWithTValue>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObjectWithTValue);
  end;
end;

function TMVCSerUnSerTestCase.GetObjectsWithStreamsList: TObjectList<TMyStreamObject>;
var
  I: Integer;
begin
  Result := TObjectList<TMyStreamObject>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObjectWithStream);
  end;
end;

procedure TMVCSerUnSerTestCase.SetSerializer(const ASerializer: IMVCSerializer);
begin
  FSerializer := ASerializer;
end;

procedure TMVCSerUnSerTestCase.SetUp;
begin
  raise Exception.Create('You should override this to use a specific MVCSerUnSer');
end;

{ TTestMultiMap }

procedure TTestMultiMap.SetUp;
begin
  inherited;

end;

procedure TTestMultiMap.TearDown;
begin
  inherited;

end;

procedure TTestMultiMap.TestInterfaceMultiMapAdd;
var
  lMultiMap: IMVCInterfaceMultiMap<IMyInterface>;
begin
  lMultiMap := TMVCInterfaceMultiMap<IMyInterface>.Create;
  Assert.AreEqual<Integer>(0, Length(lMultiMap.Keys));
  lMultiMap.Clear;
  Assert.isFalse(lMultiMap.Contains('key1'));
  lMultiMap.Add('key1', TMyIntfObject.Create(1, 'value1'));
  Assert.isTrue(lMultiMap.Contains('key1'));
  Assert.areEqual<Integer>(1, lMultiMap.GetItems('key1').Count);
  lMultiMap.Add('key1', TMyIntfObject.Create(2, 'value2'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual('value1', lMultiMap.GetItems('key1')[0].GetDescription);
  Assert.areEqual('value2', lMultiMap.GetItems('key1')[1].GetDescription);
  lMultiMap.Add('key2', TMyIntfObject.Create(1, 'value3'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual<Integer>(1, lMultiMap.GetItems('key2').Count);
end;

procedure TTestMultiMap.TestInterfaceMultiMapRemove;
var
  lMultiMap: IMVCInterfaceMultiMap<IMyInterface>;
begin
  lMultiMap := TMVCInterfaceMultiMap<IMyInterface>.Create;
  lMultiMap.Remove('not valid');
  lMultiMap.Add('key1', TMyIntfObject.Create(1, 'value1'));
  lMultiMap.Add('key1', TMyIntfObject.Create(2, 'value2'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.isTrue(lMultiMap.Contains('key1'));
  lMultiMap.Remove('key1');
  Assert.isFalse(lMultiMap.Contains('key1'));
end;

procedure TTestMultiMap.TestObjectMultiMapAdd;
var
  lMultiMap: IMVCObjectMultiMap<TMyClass>;
begin
  lMultiMap := TMVCObjectMultiMap<TMyClass>.Create;
  Assert.AreEqual<Integer>(0, Length(lMultiMap.Keys));
  lMultiMap.Clear;
  Assert.isFalse(lMultiMap.Contains('key1'));
  lMultiMap.Add('key1', TMyClass.Create(1, 'value1'));
  Assert.isTrue(lMultiMap.Contains('key1'));
  Assert.areEqual<Integer>(1, lMultiMap.GetItems('key1').Count);
  lMultiMap.Add('key1', TMyClass.Create(2, 'value2'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual('value1', lMultiMap.GetItems('key1')[0].Description);
  Assert.areEqual('value2', lMultiMap.GetItems('key1')[1].Description);
  lMultiMap.Add('key2', TMyClass.Create(1, 'value3'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual<Integer>(1, lMultiMap.GetItems('key2').Count);
end;

procedure TTestMultiMap.TestObjectMultiMapRemove;
var
  lMultiMap: IMVCObjectMultiMap<TMyClass>;
begin
  lMultiMap := TMVCObjectMultiMap<TMyClass>.Create;
  lMultiMap.Remove('not valid');
  lMultiMap.Add('key1', TMyClass.Create(1, 'value1'));
  lMultiMap.Add('key1', TMyClass.Create(2, 'value2'));
  Assert.areEqual<Integer>(2, lMultiMap.GetItems('key1').Count);
  Assert.isTrue(lMultiMap.Contains('key1'));
  lMultiMap.Remove('key1');
  Assert.isFalse(lMultiMap.Contains('key1'));
end;

{ TTestNameCase }

procedure TTestNameCase.SetupFixture;
begin
  fOrigDATA[1] := 'one_two_3or4';
  fOrigDATA[2] := 'ONE_TWO_THREE';
  fOrigDATA[3] := 'JustOne';
  fOrigDATA[4] := '_with__underscores_';
  fOrigDATA[5] := 'oneTwo___three04';

  fOutDATA[1][ncAsIs] := fOrigDATA[1];
  fOutDATA[2][ncAsIs] := fOrigDATA[2];
  fOutDATA[3][ncAsIs] := fOrigDATA[3];
  fOutDATA[4][ncAsIs] := fOrigDATA[4];
  fOutDATA[5][ncAsIs] := fOrigDATA[5];

  fOutDATA[1][ncUpperCase] := 'ONE_TWO_3OR4';
  fOutDATA[2][ncUpperCase] := 'ONE_TWO_THREE';
  fOutDATA[3][ncUpperCase] := 'JUSTONE';
  fOutDATA[4][ncUpperCase] := '_WITH__UNDERSCORES_';
  fOutDATA[5][ncUpperCase] := 'ONETWO___THREE04';

  fOutDATA[1][ncLowerCase] := 'one_two_3or4';
  fOutDATA[2][ncLowerCase] := 'one_two_three';
  fOutDATA[3][ncLowerCase] := 'justone';
  fOutDATA[4][ncLowerCase] := '_with__underscores_';
  fOutDATA[5][ncLowerCase] := 'onetwo___three04';

  fOutDATA[1][ncCamelCase] := 'oneTwo3Or4';
  fOutDATA[2][ncCamelCase] := 'oneTwoThree';
  fOutDATA[3][ncCamelCase] := 'justOne';
  fOutDATA[4][ncCamelCase] := 'WithUnderscores';
  fOutDATA[5][ncCamelCase] := 'oneTwoThree04';

  fOutDATA[1][ncPascalCase] := 'OneTwo3Or4';
  fOutDATA[2][ncPascalCase] := 'OneTwoThree';
  fOutDATA[3][ncPascalCase] := 'JustOne';
  fOutDATA[4][ncPascalCase] := 'WithUnderscores';
  fOutDATA[5][ncPascalCase] := 'OneTwoThree04';

  fOutDATA[1][ncSnakeCase] := 'one_two_3_or_4';
  fOutDATA[2][ncSnakeCase] := 'one_two_three';
  fOutDATA[3][ncSnakeCase] := 'just_one';
  fOutDATA[4][ncSnakeCase] := '_with_underscores_';
  fOutDATA[5][ncSnakeCase] := 'one_two_three_04';

end;

procedure TTestNameCase.TestNameCase;
var
  I: Integer;
  lNameCaseIdx: TMVCNameCase;
  lOrig: string;
  lOutData: string;
  lActualOutData: string;
begin
  for lNameCaseIdx := ncAsIs to ncSnakeCase do
  begin
    for I := 1 to 5 do
    begin
      lOrig := fOrigDATA[I];
      lOutData := fOutDATA[I][lNameCaseIdx];
      lActualOutData := TMVCSerializerHelper.ApplyNameCase(lNameCaseIdx, lOrig);
      Assert.areEqual(lOutData, lActualOutData, False, lOrig + ' for ' + GetEnumName(TypeInfo(TMVCNameCase),
        Ord(lNameCaseIdx)));
    end;
  end;
end;

procedure TTestNameCase.TestSnakeCase(const AValue1, AValue2: string);
begin
  Assert.areEqual(AValue2, SnakeCase(AValue1));
end;

{ TTestCryptUtils }

procedure TTestCryptUtils.SetupFixture;
begin
  MVCCryptInit;
end;

procedure TTestCryptUtils.TestPBKDF2_SHA1;
var
  P: TBytes;
  S: TBytes;
  K: TBytes;
begin
  // https://www.freecodeformat.com/pbkdf2.php
  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($78, $57, $8E, $5A, $5D, $63, $CB, $06);

  K := PBKDF2(P, S, 2048, 24);
  Assert.areEqual('BFDE6BE94DF7E11DD409BCE20A0255EC327CB936FFE93643', BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74);

  K := PBKDF2(P, S, 1, 20);
  Assert.areEqual('0C60C80F961F0E71F3A9B524AF6012062FE037A6', BytesToHexString(K));
  K := PBKDF2(P, S, 2, 20);
  Assert.areEqual('EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957', BytesToHexString(K));
  K := PBKDF2(P, S, 4096, 20);
  Assert.areEqual('4B007901B765489ABEAD49D926F721D065A429C1', BytesToHexString(K));
  // K := PBKDF2(P, S, 16777216, 20);
  // Assert.AreEqual('EEFE3D61CD4DA4E4E9945B3D6BA2158C2634E984', BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64, $50, $41, $53, $53, $57, $4F, $52, $44, $70, $61, $73, $73,
    $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74,
    $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74);

  K := PBKDF2(P, S, 4096, 25);
  Assert.areEqual('3D2EEC4FE41C849B80C8D83662C0E44A8B291A964CF2F07038', BytesToHexString(K));
end;

procedure TTestCryptUtils.TestPBKDF2_SHA256;
var
  lPassword: string;
  lSalt: string;
  lOut: TBytes;
begin
  lPassword := 'daniele.teti';
  lSalt := 'thisissomesalt';
  lOut := PBKDF2(TEncoding.ASCII.GetBytes(lPassword), TEncoding.ASCII.GetBytes(lSalt), 50, 512 div 8);
  // https://www.freecodeformat.com/pbkdf2.php
  Assert.areEqual
    ('caca227458fe66cf8c19f2d943190feca54fd403b966189d6c7befc3bc856e2d5218d825e91912058fdbdb488dbe4ae3e7be5f59318b03d805857440017ee440',
    BytesToHexString(lOut));
end;

{ TTestUTC }

procedure TTestUTC.TestStringToDateTime_Local;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // Local time
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12');
  Assert.areEqual<TDateTime>(EncodeDateTime(2020, 11, 4, 12, 12, 12, 0), lDate);

  // UTC with no time zone (in a DST period)
  lDate := ISOTimeStampToDateTime('2020-08-15T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 8, 15, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in DST period)');


  // UTC with no time zone (in no DST period)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 11, 04, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in no DST period)');
end;

procedure TTestUTC.TestDteToStringAndBack;
begin
  var lDate := EncodeDateTime(2011,11,17,12,0,0,0);
  var lDateStr := DateTimeToISOTimeStamp(lDate);
  var lDate2 := ISOTimeStampToDateTime(lDateStr);
  Assert.AreEqual(lDate, lDate2);
end;

procedure TTestUTC.TestStringToDateTime_in_DST_period;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // UTC with no time zone (in a DST period)
  lDate := ISOTimeStampToDateTime('2020-08-15T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 8, 15, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in DST period)');
end;

procedure TTestUTC.TestStringToDateTime_in_no_DST_period;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // UTC with no time zone (in no DST period)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 11, 04, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in no DST period)');
end;


procedure TTestUTC.TestStringToDateTime_Mumbai;
var
  lDate: TDateTime;
begin
  // UTC "+05:30" for Mumbai (UTC+05:30)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12+05:30');
  Assert.areEqual(DateTimeToStr(EncodeDateTime(2020, 11, 4, 7, 42, 12, 0)), DateTimeToStr(lDate));
end;

procedure TTestUTC.TestStringToDateTime_NewYork;
var
  lDate: TDateTime;
begin
  // UTC "−05:00" for New York on standard time (UTC-05:00)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12-05:00');
  Assert.areEqual(DateTimeToStr(EncodeDateTime(2020, 11, 4, 18, 12, 12, 0)), DateTimeToStr(lDate));
end;

{ TTestLRUCache }

procedure TTestLRUCache.TestPutGet;
var
  lCache: TMVCLRUCache<TMyObject>;
  I: Integer;
  lMyObj: TMyObject;
  lItemIndex: UInt64;
begin
  lCache := TMVCLRUCache<TMyObject>.Create(5);
  try
    lCache.Lock;
    try
      for I := 1 to 100 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;

      for I := 1 to 100 do
      begin
        lMyObj := TMyObject.Create;
        lMyObj.PropString := I.ToString;
        lCache.Put(I.ToString, lMyObj);
        Assert.isTrue(lCache.Contains(I.ToString, lItemIndex));
        Assert.isTrue(lCache.TryGet(I.ToString, lMyObj));
        Assert.AreEqual(I.ToString, lMyObj.PropString);
      end;
      Assert.areEqual(5, lCache.Size);
    finally
      lCache.UnLock;
    end;
    lCache.Lock;
    try
      Assert.isTrue(lCache.Contains('100', lItemIndex));
      Assert.isTrue(lCache.Contains('99', lItemIndex));
      Assert.isTrue(lCache.Contains('98', lItemIndex));
      Assert.isTrue(lCache.Contains('97', lItemIndex));
      Assert.isTrue(lCache.Contains('96', lItemIndex));
      for I := 95 downto -10 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;
      for I := 101 to 105 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;
    finally
      lCache.UnLock;
    end;
  finally
    lCache.Free;
  end;
end;

procedure TTestLRUCache.TestPutGet_Check_No_AV;
var
  lCache: TMVCLRUCache<TMyObject>;
  lMyObj: TMyObject;
  lProducer1, lProducer2, lProducer3, lProducer4: ITask;
  lProcProducer, lProcConsumer: TProc;
  lConsumer1, lConsumer2, lConsumer3, lConsumer4: ITask;
begin
  lCache := TMVCLRUCache<TMyObject>.Create(10);
  try
    lProcProducer := procedure
      var
        T, J: Integer;
        lKey: string;
      begin
        for T := 1 to 50 do
        begin
          Sleep(T * 5);
          lCache.Lock;
          try
            for J := 1 to 100 do
            begin
              lMyObj := TMyObject.Create;
              lKey := T.ToString + '|' + J.ToString;
              lMyObj.PropString := lKey;
              lCache.Put(lKey, lMyObj);
            end;
          finally
            lCache.UnLock;
          end;
        end;
      end;

    lProcConsumer := procedure
      var
        T, J: Integer;
        lKey: string;
      begin
        for T := 1 to 20 do
        begin
          Sleep(T * 10);
          lCache.Lock;
          try
            for J := T to T + 100 do
            begin
              lKey := T.ToString + '|' + J.ToString;
              lCache.TryGet(lKey, lMyObj);
            end;
          finally
            lCache.UnLock;
          end;
        end;
      end;

    lProducer1 := TTask.Run(lProcProducer).Start; // Thread
    lProducer2 := TTask.Run(lProcProducer).Start; // Thread
    lProducer3 := TTask.Run(lProcProducer).Start; // Thread
    lProducer4 := TTask.Run(lProcProducer).Start; // Thread

    lConsumer1 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer2 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer3 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer4 := TTask.Run(lProcConsumer).Start; // Thread
    TTask.WaitForAll([lProducer1, lProducer2, lProducer3, lProducer4, lConsumer1, lConsumer2, lConsumer3, lConsumer4]);
    Assert.Pass('No Exception raised');
  finally
    lCache.Free;
  end;
end;

{ TTestDotEnv }

function Are2FilesEqual(const File1, File2: TFileName): Boolean;
var
  ms1, ms2: TMemoryStream;
begin
  Result := False;
  ms1 := TMemoryStream.Create;
  try
    ms1.LoadFromFile(File1);
    ms2 := TMemoryStream.Create;
    try
      ms2.LoadFromFile(File2);
      if ms1.Size = ms2.Size then
      begin
        Result := CompareMem(ms1.Memory, ms2.memory, ms1.Size);
      end;
    finally
      ms2.Free;
    end;
  finally
    ms1.Free;
  end
end;

procedure TTestDotEnv.TestRebuild;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('dev').Build('..\dotEnv');
  lDotEnv.Rebuild;
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt')),
    'Files are different after rebuild');
end;

procedure TTestDotEnv.TestRequiredKeys;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('typedenvtest').Build('..\dotEnv');
  lDotEnv.RequireKeys(['key1','key2']);

  Assert.WillRaiseWithMessage(
    procedure
    begin
      lDotEnv.RequireKeys(['key_foo','key2']);
    end, EMVCDotEnv, 'Required keys not found: key_foo');

  Assert.WillRaiseWithMessage(
    procedure
    begin
      //key8 if defined but value is empty. It is considered unexistents.
      lDotEnv.RequireKeys(['key1','key2','key8']);
    end, EMVCDotEnv, 'Required keys not found: key8');
end;

procedure TTestDotEnv.TestSkipDefaultWithDevAndTestProfile;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv
    .SkipDefaultEnv
    .UseProfile('dev')
    .UseProfile('test')
    .Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-skip-default-profile-dev-and-test.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-skip-default-profile-dev-and-test.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-skip-default-profile-dev-and-test.test.txt')),
    'Files are different');
end;

procedure TTestDotEnv.TestTypedEnv;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('typedenvtest').Build('..\dotEnv');
  Assert.AreEqual('value1', lDotEnv.Env('key1'));
  Assert.AreEqual('value2', lDotEnv.Env('key2'));
  Assert.AreEqual('value3', lDotEnv.Env('key3'));
  Assert.AreEqual('value4', lDotEnv.Env('key4'));
  Assert.AreEqual('value2.1', lDotEnv.Env('key2.1'));
  Assert.AreEqual('value2.1|value2.2|value2.3|value3', lDotEnv.Env('key2.4'));
  ////////////
  ///  EnvTyped
  ////////////
  Assert.AreEqual(123, lDotEnv.Env('key_int', 0));
  Assert.AreEqual<Double>(12.3, lDotEnv.Env('key_float1', 0.0));
  Assert.AreEqual(12, lDotEnv.Env('key_float2', 0));
  Assert.IsTrue(lDotEnv.Env('key_boolean1', False));
  Assert.IsFalse(lDotEnv.Env('key_boolean2', True));
  Assert.IsTrue(lDotEnv.Env('key_boolean3', False));
  Assert.IsFalse(lDotEnv.Env('key_boolean4', True));
  Assert.IsTrue(lDotEnv.Env('key_boolean5', True)); //default
  Assert.IsTrue(lDotEnv.Env('key_boolean6', True)); //default
  Assert.IsTrue(lDotEnv.Env('key_boolean7', False));
  Assert.IsFalse(lDotEnv.Env('key_boolean8', True));
end;

procedure TTestDotEnv.TestWithDevAndTestProfile;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('dev').UseProfile('test').Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt')),
    'Files are different');
  lDotEnv.Rebuild;
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt')),
    'Files are different after rebuild');
end;

procedure TTestDotEnv.TestWithDevProfile;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('dev').Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt')),
    'Files are different');
end;

procedure TTestDotEnv.TestWithoutProfiles;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.test.txt')),
    'Files are different');
end;

{ TTestDotEnvParser }

procedure TTestDotEnvParser.TestErrorLineDetect01;
const
  DOTENVCODE =
  {1}  'key1= "' + sLineBreak +
  {2}  'hello' + sLineBreak +
  {3}  '\"cruel\"' + sLineBreak +
  {4}  'world' + sLineBreak +
  {5}  '  \"' + sLineBreak +
  {6}  '# comment' + sLineBreak +
  {7}  'ke y4 = "v${USERNAME}alue4 "' + sLineBreak + // <-- error should be detected at line 7
  {8}  'value3 = 123' + sLineBreak;

begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      Assert.WillRaise(
        procedure
        begin
          lParser.Parse(lDict, DOTENVCODE)
        end, nil, 'Error: Expected "=" - got "$" at line: 7');
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestErrorLineDetect02;
const
  DOTENVCODE =
   {1} '#The DB username' + sLineBreak +
   {2} 'db user=my_user' + sLineBreak +
   {3} '';
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      try
        lParser.Parse(lDict, DOTENVCODE);
        Assert.Fail('Exception not raised in case of wrong .env');
      except
        on E: Exception do
        begin
          Assert.AreEqual('Error: Expected "=" - got "u" at line: 2 near "user=my_user"', E.Message);
        end;
      end;
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestInLineComments;
const
  DOTENVCODE =
    '#comment1' + sLineBreak +
    '#comment2' + sLineBreak +
    'key1= "value1" #inline comment' + sLineBreak +
    ';comment3' + sLineBreak +
    'key2 = ''value2'' #inline comment' + sLineBreak +
    ';comment' + sLineBreak +
    'key3 = value3 #inline comment' + sLineBreak +
    'key4 = " value4 " #inline comment' + sLineBreak +
    ';commentX';

begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('value3', lDict['key3']);
      Assert.AreEqual(' value4 ', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestKeyValue;
const
  DOTENVCODE = 'key1=value1' + sLineBreak + 'key2 = value2 with another value' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2 with another value', lDict['key2']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestKeyValueWithQuotedValues;
const
  DOTENVCODE =
    'key1= "value1"' + sLineBreak +
    'key2 = ''value2''' + sLineBreak +
    'key3 = "uno''due''"' + sLineBreak +
    'key4 = ''uno"due"''' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('uno''due''', lDict['key3']);
      Assert.AreEqual('uno"due"', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestValueWithMultiline;
const
  DOTENVCODE =
    'key1= "value1' + sLineBreak +
    'value2' + sLineBreak +
    'value3" # comment' + sLineBreak +
    'key2 = value2' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1' + slinebreak + 'value2' + sLineBreak + 'value3', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestValueWithMultilineEscaped;
const
  DOTENVCODE =
    'key1= "value1' + sLineBreak +
    'value\"yes\"2' + sLineBreak +
    'value\"3\"" # comment' + sLineBreak +
    'key2 = value2' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1' + slinebreak + 'value"yes"2' + sLineBreak + 'value"3"', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestVarPlaceHolders;
const
  DOTENVCODE =
    '#comment1' + sLineBreak +
    '#comment2' + sLineBreak +
    'key1= "value1"' + sLineBreak +
    ';comment3' + sLineBreak +
    'key2 = ''value2''' + sLineBreak +
    ';comment' + sLineBreak +
    'key3 = |${key1}|${key2}|' + sLineBreak +
    'key4 = value4' + sLineBreak +
    ';commentX';

begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('|${key1}|${key2}|', lDict['key3']);
      Assert.AreEqual('value4', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestWithBadNames;
const
  DOTENVCODE = 'key1=value1' + sLineBreak + '3key2 = 12';
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      Assert.WillRaise(
      procedure
      begin
        lParser.Parse(lDict, DOTENVCODE);
      end,
      EMVCDotEnvParser);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestWithEmptyValue;
const
  DOTENVCODE = 'key1=value1' + sLineBreak + 'key2 = ' + sLineBreak + 'key3 = xyz ' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('', lDict['key2']);
      Assert.AreEqual('xyz', lDict['key3']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;


{ TTestSqids }

procedure TTestSqids.TestSingle;
begin
  Assert.AreEqual('Im1JUf',TMVCSqids.IntToSqid(1)); {https://sqids.org/playground}
  Assert.AreEqual<Integer>(1, TMVCSqids.SqidToInt(TMVCSqids.IntToSqid(1)));
end;

{ TTestRQLCompiler }

procedure TTestRQLCompiler.TestFileFixtures;
var
  lParser: TRQL2SQL;
  lSQL, lBasePath: string;
begin
  lBasePath := AppPath;
  lParser := TRQL2SQL.Create;
  try
    for var lCompName in TRQLCompilerRegistry.Instance.RegisteredCompilers do
    begin
      var lComp := TRQLCompilerRegistry.Instance.GetCompiler(lCompName).Create(nil);
      try
        Assert.IsNotNull(lComp, 'Cannot create compiler ' + lCompName);

        var lRQLs := TFile.ReadAllLines(TPath.Combine(lBasePath, '..\RQLFixtures\RQL_' + lComp.ClassName + '.fixture'));
        var lSQLs := TFile.ReadAllLines(TPath.Combine(lBasePath, '..\RQLFixtures\SQL_' + lComp.ClassName + '.fixture'));
        Assert.AreEqual(Length(lRQLs), Length(lSQLs), 'Test case for RQL different from test cases for SQL, for compiler ' + lComp.ClassName);
        for var I := 0 to Length(lRQLs) - 1 do
        begin
          try
            lParser.Execute(lRQLs[I], lSQL, lComp);
          except
            on E: Exception do
            begin
              lSQL := 'ERROR:' + E.Message;
            end;
          end;
          Assert.AreEqual(lSQLs[I], lSQL, 'Wrong compilation for "' + lSQLs[I] + '" - Compiler ' + lComp.ClassName);
        end;
      finally
        lComp.Free;
      end;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestRQLCompiler.UnmappedFieldsCanBeRefused;
var
  lParser: TRQL2SQL;
  lComp: TRQLCompiler;
  lSQL: string;
  lMapping: TMVCFieldsMapping;
  lSavedValve: Boolean;
begin
  { A name the mapping does not know is passed through to SQL verbatim. That is
    deliberate - the mapping is keyed on the Delphi field name and its alias, never
    on the database column, so filtering on a column the entity does not declare
    works today and people rely on it. What was missing is the ability to say no.
    The default stays open; 4.0 will flip it. }
  SetLength(lMapping, 1);
  lMapping[0].InstanceFieldName := 'code';
  lMapping[0].DatabaseFieldName := 'CODICE';
  lMapping[0].Alias := 'code';

  lParser := TRQL2SQL.Create;
  try
    lComp := TRQLCompilerRegistry.Instance.GetCompiler('sqlite').Create(lMapping);
    try
      lParser.Execute('eq(secret_column,1)', lSQL, lComp);
      Assert.Contains(lSQL, 'secret_column', 'the pass-through is the 3.4 behaviour');

      lSavedValve := TRQLCompiler.AllowUnmappedRQLFields;
      TRQLCompiler.AllowUnmappedRQLFields := False;
      try
        Assert.WillRaise(
          procedure
          begin
            lParser.Execute('eq(secret_column,1)', lSQL, lComp);
          end, ERQLException,
          'with the valve closed an undeclared column must not reach the SQL');
        lParser.Execute('eq(code,1)', lSQL, lComp);
        Assert.Contains(lSQL, 'CODICE', 'a mapped field must still compile');
      finally
        TRQLCompiler.AllowUnmappedRQLFields := lSavedValve;
      end;
    finally
      lComp.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestRQLCompiler.Test_MySQL_EscapesBackslash_PreventsSQLInjection;
var
  lParser: TRQL2SQL;
  lComp: TRQLCompiler;
  lSQL: string;
begin
  // On MySQL/MariaDB a backslash is a string-escape char under the default
  // sql_mode. A value such as  \'  would otherwise break out of the string
  // literal (the \' becomes a literal quote, then the doubled quote closes
  // the string). The compiler must emit '\\' for every backslash so the
  // value can never terminate the literal early.
  lParser := TRQL2SQL.Create;
  try
    lComp := TRQLCompilerRegistry.Instance.GetCompiler('mysql').Create(nil);
    try
      lParser.Execute('eq(nome,"a\'' OR 1=1")', lSQL, lComp);
      Assert.IsTrue(lSQL.Contains('a\\'),
        'MySQL RQL string values must double backslashes to prevent SQL injection; got: ' + lSQL);
    finally
      lComp.Free;
    end;
  finally
    lParser.Free;
  end;
end;

{ TTestSecurityHelpers }

procedure TTestSecurityHelpers.MVCStripCRLF_RemovesCRandLF;
begin
  // A CR/LF in a header value must not survive into the response, otherwise it
  // injects a new header (HTTP response splitting).
  Assert.AreEqual('abcX-Injected: evil', MVCStripCRLF('abc'#13#10'X-Injected: evil'));
  Assert.AreEqual('abc', MVCStripCRLF('abc'#13));
  Assert.AreEqual('abc', MVCStripCRLF('abc'#10));
  Assert.AreEqual('a normal value', MVCStripCRLF('a normal value'));
end;

procedure TTestSecurityHelpers.MVCMatchCORSOrigin_ReflectsOnlyMatchingOrigin;
begin
  // A matching origin is echoed back; an unlisted one yields no ACAO; a
  // configured wildcard yields '*'; the whole list is never emitted verbatim.
  Assert.AreEqual('https://b.com', MVCMatchCORSOrigin('https://a.com, https://b.com', 'https://b.com'));
  Assert.AreEqual('', MVCMatchCORSOrigin('https://a.com, https://b.com', 'https://evil.com'),
    'an unlisted origin must not be reflected');
  Assert.AreEqual('*', MVCMatchCORSOrigin('*', 'https://anything.example'));
  Assert.AreEqual('', MVCMatchCORSOrigin('https://a.com', ''), 'no Origin header -> no ACAO');
end;

procedure TTestSecurityHelpers.MVCCORSAllowsCredentials_NeverOnAWildcardOrigin;
begin
  { A browser rejects Access-Control-Allow-Credentials on a wildcard origin, so
    emitting it there advertises something no client can use - and the day the
    wildcard is replaced by a real origin the header starts meaning something.
    The middleware and the filter must answer the same way. }
  Assert.IsFalse(MVCCORSAllowsCredentials(True, '*'), 'credentials on a wildcard origin');
  Assert.IsFalse(MVCCORSAllowsCredentials(True, ''), 'credentials with no allowed origin');
  Assert.IsFalse(MVCCORSAllowsCredentials(False, 'https://app.example'));
  Assert.IsTrue(MVCCORSAllowsCredentials(True, 'https://app.example'));
end;

procedure TTestSecurityHelpers.TMVCFormFile_SafeFileName_StripsPathComponents;
  function SafeNameFor(const AClientName: string): string;
  var
    lFile: TMVCFormFile;
  begin
    lFile := TMVCFormFile.Create('field', AClientName, 'application/octet-stream', nil);
    try
      Result := lFile.SafeFileName;
    finally
      lFile.Free;
    end;
  end;
begin
  // The attacker-controlled filename must never carry directory components.
  Assert.AreEqual('evil.exe', SafeNameFor('..\..\..\windows\system32\evil.exe'));
  Assert.AreEqual('passwd', SafeNameFor('../../etc/passwd'));
  Assert.AreEqual('path.txt', SafeNameFor('C:\abs\path.txt'));
  Assert.AreEqual('normal.txt', SafeNameFor('normal.txt'));
  Assert.AreEqual('', SafeNameFor('..'));
end;

procedure TTestSecurityHelpers.MVCResolveClientIP_IgnoresForwardedHeadersUnlessTrusted;
begin
  // Not trusted (default): forged X-Forwarded-For / X-Real-IP are ignored, the
  // real peer IP wins.
  Assert.AreEqual('10.0.0.1', MVCResolveClientIP('1.2.3.4', '9.9.9.9', '10.0.0.1', False),
    'forwarded headers must be ignored when proxies are not trusted');
  // Trusted: the first X-Forwarded-For hop wins, then X-Real-IP, then the peer.
  Assert.AreEqual('1.2.3.4', MVCResolveClientIP('1.2.3.4, 5.6.7.8', '9.9.9.9', '10.0.0.1', True));
  Assert.AreEqual('9.9.9.9', MVCResolveClientIP('', '9.9.9.9', '10.0.0.1', True));
  Assert.AreEqual('10.0.0.1', MVCResolveClientIP('', '', '10.0.0.1', True));
end;

procedure TTestSecurityHelpers.MVCRedactSecret_MasksCredentialKeepsScheme;
begin
  // The credential must never survive into a log; the scheme is kept for triage.
  Assert.AreEqual('Bearer ***', MVCRedactSecret('Bearer eyJhbGciOiJIUzI1NiJ9.payload.sig'));
  Assert.AreEqual('Basic ***', MVCRedactSecret('Basic dXNlcjpwYXNzd29yZA=='));
  Assert.AreEqual('***', MVCRedactSecret('rawtokenwithoutscheme'));
  Assert.AreEqual('', MVCRedactSecret(''));
end;

{ TTestWizardSecurityDefaults }

function TTestWizardSecurityDefaults.TemplatesDir: string;
begin
  { The test executable lives in unittests\general\TestClient\binNN. }
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
    '..\..\..\..\ideexpert\templates'));
end;

procedure TTestWizardSecurityDefaults.GeneratedProjectsSetTheJWTCookieSecure;
var
  lDir: string;
  lFile: string;
begin
  { M-16. The framework's own default is Secure=True; the wizard used to override
    it to False in both scaffolds and write JWT_COOKIE_SECURE=false into the
    generated .env, so every generated web app shipped its authentication cookie
    in the clear unless somebody remembered to change it. This reads the template
    sources, so flipping the default back fails here. }
  lDir := TemplatesDir;
  if not TDirectory.Exists(lDir) then
  begin
    { Running from a packaged copy without the wizard sources - say so instead of
      passing quietly. }
    Assert.Pass('wizard templates not present next to this test (' + lDir + ')');
    Exit;
  end;

  for lFile in TArray<string>.Create('engineconfig.pas.tpro', 'webmodule.pas.tpro') do
    Assert.IsTrue(
      TFile.ReadAllText(TPath.Combine(lDir, lFile))
        .Contains('dotEnv.Env(''JWT_COOKIE_SECURE'', True)'),
      lFile + ' must default the JWT cookie to Secure');

  Assert.IsTrue(
    TFile.ReadAllText(TPath.Combine(lDir, 'dotenv.tpro'))
      .Contains('JWT_COOKIE_SECURE=true'),
    'the generated .env must default JWT_COOKIE_SECURE to true');
end;

procedure TTestWizardSecurityDefaults.GeneratedProjectsSetTheSessionCookieSecure;
var
  lDir: string;
  lFile: string;
begin
  { The framework default for the session cookie's Secure attribute is False,
    because a Secure cookie is not sent over plain HTTP and flipping the default
    would break every development setup on upgrade. Generated projects are new,
    so they get it turned on, with a .env switch for local HTTP work - the same
    shape as JWT_COOKIE_SECURE. }
  lDir := TemplatesDir;
  if not TDirectory.Exists(lDir) then
  begin
    Assert.Pass('wizard templates not present next to this test (' + lDir + ')');
    Exit;
  end;

  for lFile in TArray<string>.Create('engineconfig.pas.tpro', 'webmodule.pas.tpro') do
    Assert.IsTrue(
      TFile.ReadAllText(TPath.Combine(lDir, lFile))
        .Contains('dotEnv.Env(''SESSION_COOKIE_SECURE'', True)'),
      lFile + ' must pass the session cookie Secure flag');

  Assert.IsTrue(
    TFile.ReadAllText(TPath.Combine(lDir, 'dotenv.tpro'))
      .Contains('SESSION_COOKIE_SECURE=true'),
    'the generated .env must default SESSION_COOKIE_SECURE to true');

  { The file-session scaffold used to pass HttpOnly=False explicitly, undoing
    the framework default on the one store that writes the id to disk. }
  for lFile in TArray<string>.Create('engineconfig.pas.tpro', 'webmodule.pas.tpro') do
    Assert.IsFalse(
      TFile.ReadAllText(TPath.Combine(lDir, lFile))
        .Contains('UseFileSessionMiddleware({{:webmodule_middleware_session_timeout}}, False'),
      lFile + ' must not turn HttpOnly off on the file session');
end;

{ TTestClientSafeExceptionMessage }

procedure TTestClientSafeExceptionMessage.FrameworkExceptionsKeepTheirMessage;
var
  lEx: EMVCException;
begin
  { The framework's own exceptions are written for the client - a 404 that said
    "Internal server error" would be a regression in the other direction. }
  lEx := EMVCException.Create('Resource not found');
  try
    Assert.AreEqual('Resource not found', MVCClientSafeExceptionMessage(lEx));
  finally
    lEx.Free;
  end;
end;

procedure TTestClientSafeExceptionMessage.ForeignExceptionsAreGenericOutsideDebug;
var
  lEx: Exception;
begin
  { M-04. The message of anything that is not ours is internal: a FireDAC error
    carries the SQL statement with real table and column names, an IO error a
    server-side absolute path. The expectation follows the build, so this test
    keeps meaning in a RELEASE build - which is the one that matters. }
  lEx := Exception.Create(
    'EFDDBEngineException: SELECT id, ssn FROM employees WHERE dept = :p1');
  try
    {$IFDEF DEBUG}
    Assert.AreEqual(lEx.Message, MVCClientSafeExceptionMessage(lEx),
      'a DEBUG build keeps the real message');
    {$ELSE}
    Assert.AreEqual('Internal server error', MVCClientSafeExceptionMessage(lEx),
      'a RELEASE build must not send an internal message to the client');
    Assert.IsFalse(MVCClientSafeExceptionMessage(lEx).Contains('SELECT'),
      'the SQL statement reached the client');
    {$ENDIF}
  finally
    lEx.Free;
  end;
end;

{ TTestStaticFileWindowsNames }

// M-11. None of these is a traversal - the resolved file really does sit under
// the document root - but Win32 normalises the name away, so each one defeats a
// deny rule written on the file name while still reaching the file system.

procedure TTestStaticFileWindowsNames.AlternateDataStreamIsRefused;
var
  lReal: string;
  lFlagged: Boolean;
begin
  { ':' is not in FInvalidPathChars, so HasValidPathChars lets an ADS through,
    and ExtractFileExt then returns '.config::$DATA' - the media-type lookup
    misses and the file goes out as application/octet-stream. }
  Assert.IsFalse(TMVCStaticContents.IsStaticFile('www', 'web.config::$DATA',
    lReal, lFlagged));
  Assert.IsTrue(lFlagged, 'an alternate data stream must be refused');
end;

procedure TTestStaticFileWindowsNames.TrailingDotOrSpaceIsRefused;
var
  lReal: string;
  lFlagged: Boolean;
begin
  { Win32 strips a trailing dot or space, so "secret.txt." opens "secret.txt". }
  TMVCStaticContents.IsStaticFile('www', 'secret.txt.', lReal, lFlagged);
  Assert.IsTrue(lFlagged, 'a trailing dot must be refused');
  TMVCStaticContents.IsStaticFile('www', 'secret.txt ', lReal, lFlagged);
  Assert.IsTrue(lFlagged, 'a trailing space must be refused');
end;

procedure TTestStaticFileWindowsNames.WildcardsAreRefused;
var
  lReal: string;
  lFlagged: Boolean;
begin
  { HasValidPathChars(True) allows '*' and '?' on purpose; a static file request
    has no business carrying either. }
  TMVCStaticContents.IsStaticFile('www', 'secret.*', lReal, lFlagged);
  Assert.IsTrue(lFlagged, 'a wildcard must be refused');
  TMVCStaticContents.IsStaticFile('www', 'secre?.txt', lReal, lFlagged);
  Assert.IsTrue(lFlagged, 'a wildcard must be refused');
end;

procedure TTestStaticFileWindowsNames.OrdinaryNamesStillPass;
var
  lReal: string;
  lFlagged: Boolean;
begin
  { The guard must not turn into an outage: an ordinary name is not flagged,
    whether or not the file happens to exist. }
  TMVCStaticContents.IsStaticFile('www', 'index.html', lReal, lFlagged);
  Assert.IsFalse(lFlagged, 'an ordinary name must not be flagged');
  TMVCStaticContents.IsStaticFile('www', 'assets/app.min.js', lReal, lFlagged);
  Assert.IsFalse(lFlagged, 'dots inside a name are ordinary');
end;

{ TTestFormFileSaveToFile }

procedure TTestFormFileSaveToFile.SaveToFileCannotLeaveTheChosenDirectory;
var
  lDir: string;
  lPayload: TStringStream;
  lFile: TMVCFormFile;
begin
  { M-15. FileName is copied verbatim out of Content-Disposition. The two-arg
    overload keeps the directory in the caller's hands and reduces the client's
    name to a leaf; the one-arg overload cannot repair a path that was already
    combined with a hostile name, so it refuses it. }
  lDir := TPath.Combine(TPath.GetTempPath, 'dmvc_upl_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(lDir);
  try
    lPayload := TStringStream.Create('payload');
    try
      lFile := TMVCFormFile.Create('doc', '..\..\evil.txt', 'text/plain', lPayload);
      try
        lFile.SaveToFile(lDir, lFile.FileName);
        Assert.IsTrue(TFile.Exists(TPath.Combine(lDir, 'evil.txt')),
          'the upload must land in the directory the caller chose');
        Assert.IsFalse(TFile.Exists(TPath.GetFullPath(
          TPath.Combine(lDir, '..\..\evil.txt'))),
          'the upload must not be written outside that directory');

        { And the one-arg overload must not silently accept the hostile path. }
        Assert.WillRaise(
          procedure
          begin
            lFile.SaveToFile(TPath.Combine(lDir, lFile.FileName));
          end, EMVCException,
          'a path carrying a ".." segment must be refused, not written');
      finally
        lFile.Free;
      end;
    finally
      lPayload.Free;
    end;
  finally
    TDirectory.Delete(lDir, True);
  end;
end;

procedure TTestFormFileSaveToFile.SaveToFileRefusesARootedClientName;
var
  lDir, lElsewhere, lHostileName: string;
  lPayload: TStringStream;
  lFile: TMVCFormFile;
begin
  { The ".." variant is not the easy one. TPath.Combine returns its SECOND
    argument verbatim when that argument is rooted (System.IOUtils,
    DoIsPathRooted), so SaveToFile(TPath.Combine(UPLOAD_DIR, Doc.FileName)) with
    a filename of "C:\inetpub\wwwroot\shell.aspx" drops UPLOAD_DIR entirely and
    writes wherever the client said - with no dot segment anywhere in the path. }
  lDir := TPath.Combine(TPath.GetTempPath, 'dmvc_upl_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  lElsewhere := TPath.Combine(TPath.GetTempPath, 'dmvc_pwn_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(lDir);
  TDirectory.CreateDirectory(lElsewhere);
  try
    lHostileName := TPath.Combine(lElsewhere, 'pwned.txt');
    lPayload := TStringStream.Create('payload');
    try
      lFile := TMVCFormFile.Create('doc', lHostileName, 'text/plain', lPayload);
      try
        Assert.WillRaise(
          procedure
          begin
            lFile.SaveToFile(TPath.Combine(lDir, lFile.FileName));
          end, EMVCException,
          'a rooted client file name must be refused, not written');
        Assert.IsFalse(TFile.Exists(lHostileName),
          'the upload escaped the directory the caller chose');

        { The two-argument overload reduces it to a leaf instead of refusing. }
        lFile.SaveToFile(lDir, lFile.FileName);
        Assert.IsTrue(TFile.Exists(TPath.Combine(lDir, 'pwned.txt')),
          'the safe overload must still save, under the leaf name');
        Assert.IsFalse(TFile.Exists(lHostileName));
      finally
        lFile.Free;
      end;
    finally
      lPayload.Free;
    end;
  finally
    TDirectory.Delete(lDir, True);
    TDirectory.Delete(lElsewhere, True);
  end;
end;

procedure TTestFormFileSaveToFile.SaveToFileAcceptsARelativeApplicationDirectory;
var
  lRelDir, lTarget: string;
  lPayload: TStringStream;
  lFile: TMVCFormFile;
begin
  { The guard must look at the client's name, not at the path: '.\uploads' is
    an ordinary application directory - it is the shape TMVCStaticFilesOptions
    itself defaults to - and a guard that refused a "." segment anywhere in the
    path turned the documented one-argument idiom into an exception. }
  lRelDir := '.' + PathDelim + 'dmvc_upl_rel_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  TDirectory.CreateDirectory(lRelDir);
  try
    lPayload := TStringStream.Create('payload');
    try
      lFile := TMVCFormFile.Create('doc', 'report.pdf', 'application/pdf', lPayload);
      try
        lTarget := TPath.Combine(lRelDir, lFile.SafeFileName);
        lFile.SaveToFile(lTarget);
        Assert.IsTrue(TFile.Exists(lTarget),
          'a relative application directory must not be refused');
      finally
        lFile.Free;
      end;
    finally
      lPayload.Free;
    end;
  finally
    TDirectory.Delete(lRelDir, True);
  end;
end;

procedure TTestFormFileSaveToFile.SaveToFileKeepsAnOrdinaryName;
var
  lDir, lTarget: string;
  lPayload: TStringStream;
  lFile: TMVCFormFile;
begin
  { The guard must not become an outage: an ordinary upload still saves, through
    either overload. }
  lDir := TPath.Combine(TPath.GetTempPath, 'dmvc_upl_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(lDir);
  try
    lPayload := TStringStream.Create('payload');
    try
      lFile := TMVCFormFile.Create('doc', 'report.pdf', 'application/pdf', lPayload);
      try
        lTarget := TPath.Combine(lDir, 'report.pdf');
        lFile.SaveToFile(lTarget);
        Assert.IsTrue(TFile.Exists(lTarget), 'an ordinary upload must still be saved');
        Assert.AreEqual('payload', TFile.ReadAllText(lTarget));

        lFile.SaveToFile(lDir, 'copy.pdf');
        Assert.IsTrue(TFile.Exists(TPath.Combine(lDir, 'copy.pdf')));
      finally
        lFile.Free;
      end;
    finally
      lPayload.Free;
    end;
  finally
    TDirectory.Delete(lDir, True);
  end;
end;

{ TTestRateLimitStoreCeiling }

procedure TTestRateLimitStoreCeiling.StoreDoesNotGrowWithoutBound;
var
  lStore: TMVCInMemoryRateLimitStorage;
  lKeepAlive: IMVCRateLimitStorage;
  I, lRemaining: Integer;
  lReset: TDateTime;
begin
  { M-10. With a client-controlled key (rlkAPIKey reads X-API-Key) a caller both
    escapes its own limit and grows this store for the whole window, while every
    request pays an O(N) scan under one lock. Only the entries already expired
    used to be dropped, and a rotating key never expires within the window.
    The ceiling is 100000; going past it must not leave the store above it. }
  lStore := TMVCInMemoryRateLimitStorage.Create;
  lKeepAlive := lStore; // refcount owns it
  for I := 1 to 100010 do
    lStore.CheckRateLimit('key-' + IntToStr(I), 100, 60, lRemaining, lReset);
  Assert.IsTrue(lStore.KeyCount <= 100000,
    'the rate limit store grew past its ceiling (' + lStore.KeyCount.ToString + ')');

  { Past the ceiling an UNKNOWN key is refused, not admitted. True means "limit
    exceeded" to every caller of IMVCRateLimitStorage, so answering False here
    would switch the limiter OFF for exactly the keys a flooder rotates through -
    the mitigation would be a better bypass than the growth it prevents. }
  Assert.IsTrue(lStore.CheckRateLimit('brand-new-key', 100, 60, lRemaining, lReset),
    'past the ceiling an unknown key must be refused, not let through');

  { A key the store already holds is unaffected: the ceiling must not lock out
    the clients that were already being counted. }
  Assert.IsFalse(lStore.CheckRateLimit('key-1', 100, 60, lRemaining, lReset),
    'a key already in the store must keep being served past the ceiling');
end;

{ TTestPathDotSegments }

procedure TTestPathDotSegments.DotSegmentsAreDetected;
begin
  Assert.IsTrue(MVCPathHasDotSegment('/public/../admin'));
  Assert.IsTrue(MVCPathHasDotSegment('/a/./b'));
  Assert.IsTrue(MVCPathHasDotSegment('/..'));
  Assert.IsTrue(MVCPathHasDotSegment('..'));
  Assert.IsTrue(MVCPathHasDotSegment('/a/b/..'));
  { Backslash too: the file-system callers pass Windows paths, and a browser
    treats a backslash in a URL as a separator anyway. }
  Assert.IsTrue(MVCPathHasDotSegment('C:\uploads\..\..\evil.txt'));
  Assert.IsTrue(MVCPathHasDotSegment('a\.\b'));
end;

procedure TTestPathDotSegments.DotsInsideNamesAreNotSegments;
begin
  { A dot inside a segment is an ordinary character. Refusing these would break
    perfectly normal URLs, which is how an over-eager check becomes an outage. }
  Assert.IsFalse(MVCPathHasDotSegment('/files/release.1.2.zip'));
  Assert.IsFalse(MVCPathHasDotSegment('/a/..hidden'));
  Assert.IsFalse(MVCPathHasDotSegment('/a/...'));
  Assert.IsFalse(MVCPathHasDotSegment('/plain/path'));
  Assert.IsFalse(MVCPathHasDotSegment(''));
end;

{ TTestSessionIDValidation }

procedure TTestSessionIDValidation.GeneratedIDsAreAccepted;
var
  I: Integer;
begin
  { Whatever the engine issues must survive its own filter, or every session
    breaks. Checked against real generated ids, not a hand-written sample. }
  for I := 1 to 50 do
    Assert.IsTrue(IsValidSessionID(GenerateSessionID),
      'a freshly generated session id was rejected');
end;

procedure TTestSessionIDValidation.TraversalShapesAreRejected;
begin
  Assert.IsFalse(IsValidSessionID('..\..\www\assets\app.js'));
  Assert.IsFalse(IsValidSessionID('../../etc/passwd'));
  Assert.IsFalse(IsValidSessionID('..'));
  { Already percent-decoded by the time the check runs. }
  Assert.IsFalse(IsValidSessionID('..%5c..%5cwww'));
end;

procedure TTestSessionIDValidation.RootedAndUNCShapesAreRejected;
begin
  { TPath.Combine returns its second argument verbatim when that one is rooted,
    so a rooted id escapes the session folder without needing any dots. }
  Assert.IsFalse(IsValidSessionID('C:\Windows\Temp\x'));
  Assert.IsFalse(IsValidSessionID('\attacker\share\x'));
  Assert.IsFalse(IsValidSessionID('/etc/passwd'));
end;

procedure TTestSessionIDValidation.EmptyAndOverlongAreRejected;
begin
  Assert.IsFalse(IsValidSessionID(''));
  Assert.IsFalse(IsValidSessionID(StringOfChar('A', 256)));
  Assert.IsTrue(IsValidSessionID(StringOfChar('A', 255)));
end;

procedure TTestSessionIDValidation.SeparatorsAndDotsAreRejected;
begin
  { A trailing dot or a colon is a path trick on Windows: trailing dots are
    stripped by the filesystem, and a colon opens an alternate data stream. }
  Assert.IsFalse(IsValidSessionID('DT1234.'));
  Assert.IsFalse(IsValidSessionID('DT1234::$DATA'));
  Assert.IsFalse(IsValidSessionID('DT 1234'));
  Assert.IsFalse(IsValidSessionID('DT-1234'));
  Assert.IsFalse(IsValidSessionID('DT_1234'));
end;

{ TTestStaticFilesTraversal }

procedure TTestStaticFilesTraversal.SiblingDirectoryEscapeIsBlocked;
var
  lBase, lDocRoot, lSibling, lRealFile: string;
  lIsTraversal, lFound: Boolean;
begin
  // Doc root "www" and a sibling "www-secret" share a string prefix. A request
  // for ../www-secret/secret.txt resolves outside the doc root but the naive
  // StartsWith(docroot) check (no path separator) accepts it. Must be blocked.
  lBase := TPath.Combine(TPath.GetTempPath, 'dmvc_trav_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  lDocRoot := TPath.Combine(lBase, 'www');
  lSibling := TPath.Combine(lBase, 'www-secret');
  TDirectory.CreateDirectory(lDocRoot);
  TDirectory.CreateDirectory(lSibling);
  TFile.WriteAllText(TPath.Combine(lSibling, 'secret.txt'), 'TOP SECRET');
  try
    lFound := TMVCStaticContents.IsStaticFile(lDocRoot, '../www-secret/secret.txt',
      lRealFile, lIsTraversal);
    Assert.IsTrue(lIsTraversal,
      'Escaping to a sibling dir sharing the doc-root prefix must be flagged as traversal');
    Assert.IsFalse(lFound, 'A file outside the doc root must not be served');
  finally
    TDirectory.Delete(lBase, True);
  end;
end;

procedure TTestStaticFilesTraversal.LegitFileInsideDocRootIsServed;
var
  lBase, lDocRoot, lRealFile: string;
  lIsTraversal, lFound: Boolean;
begin
  lBase := TPath.Combine(TPath.GetTempPath, 'dmvc_trav_' +
    TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  lDocRoot := TPath.Combine(lBase, 'www');
  TDirectory.CreateDirectory(lDocRoot);
  TFile.WriteAllText(TPath.Combine(lDocRoot, 'index.html'), '<html/>');
  try
    lFound := TMVCStaticContents.IsStaticFile(lDocRoot, 'index.html', lRealFile, lIsTraversal);
    Assert.IsFalse(lIsTraversal, 'A file inside the doc root must not be flagged as traversal');
    Assert.IsTrue(lFound, 'A file inside the doc root must be served');
  finally
    TDirectory.Delete(lBase, True);
  end;
end;

{ TTestGenericNullables }

procedure TTestGenericNullables.TestGenericNullables;
var
  lNullInt: Nullable<Integer>;
  lTmpInt: Integer;
begin
  Assert.IsTrue(lNullInt.IsNull);
  Assert.IsFalse(lNullInt.HasValue);
  lNullInt := 123;
  Assert.IsFalse(lNullInt.IsNull);
  Assert.IsTrue(lNullInt.HasValue);

  Assert.AreEqual(123, Integer(lNullInt));
  lTmpInt := lNullInt;
  Assert.AreEqual(123, lTmpInt);

  lNullInt := nil;
  Assert.IsTrue(lNullInt.IsNull);
  Assert.IsFalse(lNullInt.HasValue);
  lNullInt := 123;
  lNullInt.Clear;
  Assert.IsTrue(lNullInt.IsNull);
  Assert.IsFalse(lNullInt.HasValue);
end;

{ TSwagBaseController }

procedure TSwagBaseController.DescribedAction;
begin
  // never called: this controller only exists to carry attributes
end;

procedure TSwagBaseController.InheritedAction;
begin
  // never called: this controller only exists to carry attributes
end;

procedure TSwagBaseController.MultiVerbAction;
begin
  // never called: this controller only exists to carry attributes
end;

procedure TSwagBaseController.NoVerbAction;
begin
  // never called: this controller only exists to carry attributes
end;

{ TTestSwaggerMetadata }

function TTestSwaggerMetadata.OperationFor(const AMethodName: string;
  const ADefaultTags: TArray<String>): TSwagPathOperation;
var
  lCtx: TRttiContext;
  lDefinitions: TObjectList<TSwagDefinition>;
begin
  lCtx := TRttiContext.Create;
  lDefinitions := TObjectList<TSwagDefinition>.Create(True);
  try
    Result := TSwagPathOperation.Create;
    try
      TMVCSwagger.FillOperationSummary(
        Result,
        lCtx.GetType(TSwagDerivedController).GetMethod(AMethodName),
        lDefinitions,
        httpGET,
        nil,
        '',
        '',
        ADefaultTags);
    except
      Result.Free;
      raise;
    end;
  finally
    lDefinitions.Free;
    lCtx.Free;
  end;
end;

procedure TTestSwaggerMetadata.InheritedActionUsesTheControllerDefaultTags;
var
  lOperation: TSwagPathOperation;
begin
  lOperation := OperationFor('InheritedAction', ['Events']);
  try
    Assert.AreEqual<Integer>(1, lOperation.Tags.Count);
    {Before the fix this was the *declaring* class, so every controller
     inheriting the action ended up sharing one meaningless tag}
    Assert.AreEqual('Events', lOperation.Tags[0]);
  finally
    lOperation.Free;
  end;
end;

procedure TTestSwaggerMetadata.InheritedActionWithoutDefaultTagsKeepsTheOldFallback;
var
  lOperation: TSwagPathOperation;
  lNoTags: TArray<String>;
begin
  SetLength(lNoTags, 0);
  lOperation := OperationFor('InheritedAction', lNoTags);
  try
    Assert.AreEqual<Integer>(1, lOperation.Tags.Count);
    Assert.AreEqual(TSwagBaseController.QualifiedClassName, lOperation.Tags[0]);
  finally
    lOperation.Free;
  end;
end;

procedure TTestSwaggerMetadata.SummaryIsFilledNotOnlyDescription;
var
  lOperation: TSwagPathOperation;
  lNoTags: TArray<String>;
begin
  SetLength(lNoTags, 0);
  lOperation := OperationFor('DescribedAction', lNoTags);
  try
    {Swagger UI titles the operation row with the summary: leaving it empty
     is what made a documented endpoint look undocumented}
    Assert.AreEqual('Streams the events', lOperation.Summary);
    Assert.AreEqual('Streams the events', lOperation.Description);
    Assert.AreEqual('Streams', lOperation.Tags[0]);
    Assert.AreEqual('streamEvents', lOperation.OperationID);
  finally
    lOperation.Free;
  end;
end;

procedure TTestSwaggerMetadata.AllowedMethodsUnionsEveryAttribute;
var
  lCtx: TRttiContext;
begin
  lCtx := TRttiContext.Create;
  try
    {Two [MVCHTTPMethod] on one action: the router unions them, and the doc
     emitters ask the router, so they cannot disagree}
    Assert.IsTrue(
      TMVCRouter.AllowedMethods(
        lCtx.GetType(TSwagDerivedController).GetMethod('MultiVerbAction').GetAttributes)
      = [httpGET, httpPOST, httpPUT]);
  finally
    lCtx.Free;
  end;
end;

procedure TTestSwaggerMetadata.AllowedMethodsDefaultsToEveryVerb;
var
  lCtx: TRttiContext;
begin
  lCtx := TRttiContext.Create;
  try
    Assert.IsTrue(
      TMVCRouter.AllowedMethods(
        lCtx.GetType(TSwagDerivedController).GetMethod('NoVerbAction').GetAttributes)
      = [httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpHEAD, httpOPTIONS, httpTRACE,
        httpQUERY]);
  finally
    lCtx.Free;
  end;
end;

procedure TTestSwaggerMetadata.VerbsOpenAPI2CannotExpressAreSkipped;
begin
  {OpenAPI 2 has no "query" path item. The mapper answers ohvNotDefined, whose
   verb string is empty - both Swagger walkers must Continue on it, or the
   emitted swagger.json carries an empty key and is malformed.}
  Assert.IsTrue(TMVCSwagger.MVCHttpMethodToSwagPathOperation(httpQUERY) = ohvNotDefined);
  Assert.areEqual('', c_SwagPathOperationHttpVerbs[ohvNotDefined]);
end;

{ TTestJSONNestingDepth }

function TTestJSONNestingDepth.NestedJSON(const ADepth: Integer;
  const AArrays: Boolean): string;
var
  I: Integer;
  lSB: TStringBuilder;
  lOpen, lClose: string;
begin
  if AArrays then
  begin
    lOpen := '[';
    lClose := ']';
  end
  else
  begin
    lOpen := '{"a":';
    lClose := '}';
  end;
  lSB := TStringBuilder.Create;
  try
    for I := 1 to ADepth do
      lSB.Append(lOpen);
    lSB.Append('1');
    for I := 1 to ADepth do
      lSB.Append(lClose);
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

procedure TTestJSONNestingDepth.TheLimitIsInPlaceAndSane;
begin
  Assert.IsTrue(JsonMaxNestingDepth > 0,
    'JsonMaxNestingDepth is disabled: the vendored JsonDataObjects.pas is ' +
    'unprotected against stack exhaustion from nested JSON. Was the depth ' +
    'patch lost in a re-sync with upstream?');
  Assert.IsTrue(JsonMaxNestingDepth <= 4096,
    'JsonMaxNestingDepth is too high to keep the parser inside the stack');
end;

procedure TTestJSONNestingDepth.AtTheLimitItParses;
var
  lJSON: TJsonBaseObject;
begin
  // The limit must not be so tight that legitimate documents break.
  lJSON := TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth, False));
  try
    Assert.IsNotNull(lJSON, 'a document at exactly the limit must parse');
  finally
    lJSON.Free;
  end;

  lJSON := TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth, True));
  try
    Assert.IsNotNull(lJSON, 'a document at exactly the limit must parse');
  finally
    lJSON.Free;
  end;
end;

procedure TTestJSONNestingDepth.PastTheLimitItRaisesInsteadOfCrashing;
begin
  Assert.WillRaise(
    procedure
    begin
      TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth + 1, False)).Free;
    end, EJsonParserException);

  Assert.WillRaise(
    procedure
    begin
      TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth + 1, True)).Free;
    end, EJsonParserException);

  Assert.WillRaise(
    procedure
    begin
      TJsonBaseObject.ParseUtf8(UTF8Encode(NestedJSON(JsonMaxNestingDepth + 1, False))).Free;
    end, EJsonParserException);

  // IsValidJSON() swallows parser exceptions and answers False. Without the
  // limit it does not answer at all: it takes the process with it.
  Assert.IsFalse(IsValidJSON(NestedJSON(JsonMaxNestingDepth + 1, False)),
    'IsValidJSON accepted a document nested past the limit');
end;

procedure TTestJSONNestingDepth.StrToJSONObjectSurvivesADeeplyNestedBody;
var
  lDeep: string;
begin
  // This is the path a request body actually travels.
  lDeep := NestedJSON(JsonMaxNestingDepth + 1, False);

  Assert.IsNull(StrToJSONObject(lDeep),
    'StrToJSONObject must answer nil on a body nested past the limit');

  Assert.WillRaise(
    procedure
    begin
      StrToJSONObject(lDeep, True).Free;
    end, EMVCDeserializationException);
end;

procedure TTestJSONNestingDepth.TheDepthCounterDoesNotLeakAcrossParses;
var
  I: Integer;
  lJSON: TJsonBaseObject;
begin
  // Every aborted parse leaves its reader with FDepth > 0. If the counter were
  // not reset per parse, the Nth valid document would start out already "deep".
  for I := 1 to 3 do
    try
      TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth + 1, False)).Free;
    except
      on EJsonParserException do
        ; // expected
    end;

  lJSON := TJsonBaseObject.Parse(NestedJSON(JsonMaxNestingDepth, False));
  try
    Assert.IsNotNull(lJSON, 'the depth counter leaked across parses');
  finally
    lJSON.Free;
  end;
end;

procedure TTestJSONNestingDepth.ClearingTheLimitDoesNotDisableIt;
var
  lSaved: Integer;
begin
  // Zeroing the limit used to mean "no limit", which put a single line of
  // configuration between an app and the crash the limit exists to prevent.
  // A cleared limit is the default one instead: it cannot be switched off.
  lSaved := JsonMaxNestingDepth;
  try
    JsonMaxNestingDepth := 0;
    Assert.WillRaise(
      procedure
      begin
        TJsonBaseObject.Parse(NestedJSON(DefaultJsonMaxNestingDepth + 1, False)).Free;
      end, EJsonParserException);

    JsonMaxNestingDepth := -1;
    Assert.WillRaise(
      procedure
      begin
        TJsonBaseObject.Parse(NestedJSON(DefaultJsonMaxNestingDepth + 1, False)).Free;
      end, EJsonParserException);
  finally
    JsonMaxNestingDepth := lSaved;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestRouting);
// TDUnitX.RegisterTestFixture(TTestMappers);
TDUnitX.RegisterTestFixture(TTestJWT);
TDUnitX.RegisterTestFixture(TTestMultiMap);
TDUnitX.RegisterTestFixture(TTestNameCase);
TDUnitX.RegisterTestFixture(TTestCryptUtils);
TDUnitX.RegisterTestFixture(TTestLRUCache);
TDUnitX.RegisterTestFixture(TTestDotEnv);
TDUnitX.RegisterTestFixture(TTestDotEnvParser);
TDUnitX.RegisterTestFixture(TTestSqids);
TDUnitX.RegisterTestFixture(TTestRQLCompiler);
TDUnitX.RegisterTestFixture(TTestGenericNullables);
TDUnitX.RegisterTestFixture(TTestWizardSecurityDefaults);
TDUnitX.RegisterTestFixture(TTestClientSafeExceptionMessage);
TDUnitX.RegisterTestFixture(TTestStaticFileWindowsNames);
TDUnitX.RegisterTestFixture(TTestFormFileSaveToFile);
TDUnitX.RegisterTestFixture(TTestRateLimitStoreCeiling);
TDUnitX.RegisterTestFixture(TTestPathDotSegments);
TDUnitX.RegisterTestFixture(TTestSessionIDValidation);
TDUnitX.RegisterTestFixture(TTestStaticFilesTraversal);
TDUnitX.RegisterTestFixture(TTestSecurityHelpers);
TDUnitX.RegisterTestFixture(TTestSwaggerMetadata);
TDUnitX.RegisterTestFixture(TTestJSONNestingDepth);

finalization

end.
