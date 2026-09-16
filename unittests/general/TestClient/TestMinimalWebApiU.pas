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

unit TestMinimalWebApiU;

interface

uses
  DUnitX.TestFramework,
  LiveServerTestU;

type
  [TestFixture]
  TTestMinimalWebApi = class(TBaseServerTest)
  public
    [Test]
    procedure Test_WebRoot_routing_returns_text_html;
    [Test]
    procedure Test_ViewData_via_Ctx_renders;
    [Test]
    procedure Test_RenderView_contentType_includes_charset;
    [Test]
    procedure Test_Form_binding_with_MVCFromContentField;
    [Test]
    procedure Test_OpenAPI_excludes_rkWeb_routes_by_default;
    [Test]
    procedure Test_OpenAPI_includes_web_route_with_WithOpenAPI_True;
    [Test]
    procedure Test_Filter_can_render_HTML_response;
    [Test]
    procedure Test_Threadvar_isolation_under_concurrent_requests;
    [Test]
    procedure Test_TArrayString_multivalue_form_binding;
    [Test]
    procedure Test_ContentNegotiation_html_accept_selects_rkWeb;
    [Test]
    procedure Test_ContentNegotiation_json_accept_selects_rkApi;
    // ----- Minimal API parity features -----
    [Test]
    procedure Test_Record_validation_rejects_invalid;
    [Test]
    procedure Test_Record_validation_accepts_valid;
    [Test]
    procedure Test_FormFile_single_arg_binds_first_file;
    [Test]
    procedure Test_FormFile_record_field_with_text_field;
    [Test]
    procedure Test_QueryString_array_binding_string_and_int;
    [Test]
    procedure Test_MapQuery_binds_the_json_body;
    [Test]
    procedure Test_MapQuery_route_does_not_answer_GET;
    [Test]
    procedure Test_Wildcard_segment_captures_rest_of_path;
    [Test]
    procedure Test_Wildcard_segment_matches_empty_tail;
    [Test]
    procedure Test_RequireRole_unauthenticated_is_401;
    [Test]
    procedure Test_RequireRole_wrong_role_is_403;
    [Test]
    procedure Test_RequireRole_correct_role_is_200;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Threading,
  System.SyncObjs,
  JsonDataObjects,
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  MVCFramework.Commons,
  TestConstsU;

procedure TTestMinimalWebApi.Test_WebRoot_routing_returns_text_html;
var
  lResp: IMVCRESTResponse;
  lCT: string;
begin
  lResp := RESTClient.Get('/minimal-web/hello');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  lCT := LowerCase(lResp.HeaderValue('Content-Type'));
  Assert.Contains(lCT, 'text/html');
  Assert.Contains(lResp.Content, 'Hello, world!');
end;

procedure TTestMinimalWebApi.Test_ViewData_via_Ctx_renders;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-web/viewdata');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'a=alpha');
  Assert.Contains(lResp.Content, 'b=beta');
end;

procedure TTestMinimalWebApi.Test_RenderView_contentType_includes_charset;
var
  lResp: IMVCRESTResponse;
  lCT: string;
begin
  lResp := RESTClient.Get('/minimal-web/hello');
  lCT := LowerCase(lResp.HeaderValue('Content-Type'));
  Assert.Contains(lCT, 'text/html');
  Assert.Contains(lCT, 'charset=utf-8');
end;

procedure TTestMinimalWebApi.Test_Form_binding_with_MVCFromContentField;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient
    .AddBodyFieldFormData('username', 'alice')
    .AddBodyFieldFormData('password', 'pw')
    .AddBodyFieldFormData('remember', 'true')
    .Post('/minimal-web/login');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'user=alice');
  // JSON booleans render as lowercase 'true'/'false' via the Mustache engine
  Assert.Contains(LowerCase(lResp.Content), 'remember=true');
end;

procedure TTestMinimalWebApi.Test_OpenAPI_excludes_rkWeb_routes_by_default;
var
  lResp: IMVCRESTResponse;
  lJson: TJsonObject;
  lPaths: TJsonObject;
begin
  lResp := RESTClient.Get('/openapi.json');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  lJson := TJsonObject.Parse(lResp.Content) as TJsonObject;
  try
    lPaths := lJson.O['paths'];
    Assert.IsTrue(lPaths.Contains('/minimal-web/api-side'),
      'API sibling must be in spec');
    Assert.IsFalse(lPaths.Contains('/minimal-web/web-side'),
      'Web sibling must NOT be in spec by default');
  finally
    lJson.Free;
  end;
end;

procedure TTestMinimalWebApi.Test_OpenAPI_includes_web_route_with_WithOpenAPI_True;
var
  lResp: IMVCRESTResponse;
  lJson: TJsonObject;
  lPaths: TJsonObject;
begin
  lResp := RESTClient.Get('/openapi.json');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  lJson := TJsonObject.Parse(lResp.Content) as TJsonObject;
  try
    lPaths := lJson.O['paths'];
    Assert.IsTrue(lPaths.Contains('/minimal-web/web-visible'),
      'Web route opted in with .WithOpenAPI(True) must be in spec');
  finally
    lJson.Free;
  end;
end;

procedure TTestMinimalWebApi.Test_Filter_can_render_HTML_response;
var
  lOk, lBlocked: IMVCRESTResponse;
begin
  lOk := RESTClient.Get('/minimal-web/filter');
  Assert.AreEqual<Integer>(200, lOk.StatusCode);
  Assert.Contains(lOk.Content, 'FILTER_OK');

  lBlocked := RESTClient.Get('/minimal-web/filter?block=1');
  Assert.AreEqual<Integer>(200, lBlocked.StatusCode);
  Assert.Contains(lBlocked.Content, 'BLOCKED');
end;

procedure TTestMinimalWebApi.Test_Threadvar_isolation_under_concurrent_requests;
const
  ITERATIONS = 50;
var
  lTasks: array of ITask;
  i: Integer;
  lErrors: TStringList;
  lLock: TCriticalSection;
begin
  lErrors := TStringList.Create;
  lLock := TCriticalSection.Create;
  try
    SetLength(lTasks, ITERATIONS);
    for i := 0 to ITERATIONS - 1 do
    begin
      lTasks[i] := TTask.Run(
        procedure
        var
          lMarker: string;
          lResp: IMVCRESTResponse;
          lClient: IMVCRESTClient;
          lExpected: string;
        begin
          // Each thread uses its own client; IMVCRESTClient is not designed
          // for cross-thread sharing of a single instance.
          lMarker := Format('m%d', [Random(1000000)]);
          lClient := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 8888);
          lClient
            .ReadTimeout(60 * 1000 * 30)
            .ProxyServer('localhost')
            .ProxyPort(8080);
          lResp := lClient.Get('/minimal-web/iso/' + lMarker);
          lExpected := Format('marker=%s', [lMarker]);
          if not lResp.Content.Contains(lExpected) then
          begin
            lLock.Enter;
            try
              lErrors.Add(Format('expected %s, status %d, body: %s',
                [lExpected, lResp.StatusCode, lResp.Content]));
            finally
              lLock.Leave;
            end;
          end;
        end);
    end;
    TTask.WaitForAll(lTasks);
    Assert.AreEqual<Integer>(0, lErrors.Count, lErrors.Text);
  finally
    lErrors.Free;
    lLock.Free;
  end;
end;

procedure TTestMinimalWebApi.Test_TArrayString_multivalue_form_binding;
var
  lResp: IMVCRESTResponse;
begin
  // Send the same 'tag' field three times - multi-value form post.
  // Verifies that [MVCFromContentField] on TArray<string> in the record
  // binds via ContentParamsMulti (mirrors the classic controller behavior).
  lResp := RESTClient
    .AddBodyFieldFormData('tag', 'alpha')
    .AddBodyFieldFormData('tag', 'beta')
    .AddBodyFieldFormData('tag', 'gamma')
    .Post('/minimal-web/multi');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'count=3');
  Assert.Contains(lResp.Content, 'first=alpha');
  Assert.Contains(lResp.Content, 'second=beta');
end;

procedure TTestMinimalWebApi.Test_ContentNegotiation_html_accept_selects_rkWeb;
var
  lResp: IMVCRESTResponse;
begin
  // Two routes share GET /minimal-web/negotiate — one rkApi (returns
  // 'negotiate-api' JSON-wrapped), one rkWeb (renders HTML containing
  // 'negotiate-web'). Browser-like Accept must select the rkWeb route.
  lResp := RESTClient
    .AddHeader('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8')
    .Get('/minimal-web/negotiate');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(LowerCase(lResp.HeaderValue('Content-Type')), 'text/html');
  Assert.Contains(lResp.Content, 'negotiate-web');
end;

procedure TTestMinimalWebApi.Test_ContentNegotiation_json_accept_selects_rkApi;
var
  lResp: IMVCRESTResponse;
begin
  // Same two routes. API-client Accept must select the rkApi route which
  // returns 'negotiate-api' through the JSON pipeline.
  lResp := RESTClient
    .AddHeader('Accept', 'application/json')
    .Get('/minimal-web/negotiate');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(LowerCase(lResp.HeaderValue('Content-Type')), 'application/json');
  Assert.Contains(lResp.Content, 'negotiate-api');
end;

// ----- Minimal API parity features -----

procedure TTestMinimalWebApi.Test_Record_validation_rejects_invalid;
var
  lResp: IMVCRESTResponse;
begin
  // MinLength(3) on the bound query field -> too short -> 422
  lResp := RESTClient.Get('/minimal-feat/validate?q=ab');
  Assert.AreEqual<Integer>(422, lResp.StatusCode);
end;

procedure TTestMinimalWebApi.Test_Record_validation_accepts_valid;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-feat/validate?q=abc');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'q=abc');
end;

procedure TTestMinimalWebApi.Test_FormFile_single_arg_binds_first_file;
var
  lResp: IMVCRESTResponse;
  lTmp: string;
begin
  lTmp := TPath.Combine(TPath.GetTempPath, 'mvc_upload_a.txt');
  TFile.WriteAllText(lTmp, 'hello-upload');
  try
    lResp := RESTClient.AddFile('doc', lTmp, 'text/plain').Post('/minimal-feat/upload1');
    Assert.AreEqual<Integer>(200, lResp.StatusCode);
    Assert.Contains(lResp.Content, 'name=mvc_upload_a.txt');
    Assert.Contains(lResp.Content, 'size=12'); // 'hello-upload' = 12 bytes
  finally
    TFile.Delete(lTmp);
  end;
end;

procedure TTestMinimalWebApi.Test_FormFile_record_field_with_text_field;
var
  lResp: IMVCRESTResponse;
  lTmp: string;
begin
  lTmp := TPath.Combine(TPath.GetTempPath, 'mvc_upload_b.txt');
  TFile.WriteAllText(lTmp, 'abc');
  try
    lResp := RESTClient
      .AddBodyFieldFormData('title', 'My Doc')
      .AddFile('doc', lTmp, 'text/plain')
      .Post('/minimal-feat/upload');
    Assert.AreEqual<Integer>(200, lResp.StatusCode);
    Assert.Contains(lResp.Content, 'title=My Doc');
    Assert.Contains(lResp.Content, 'file=mvc_upload_b.txt');
  finally
    TFile.Delete(lTmp);
  end;
end;

procedure TTestMinimalWebApi.Test_QueryString_array_binding_string_and_int;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-feat/tags?tag=red&tag=green&tag=blue&id=1&id=2&id=4');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'tags=3');
  Assert.Contains(lResp.Content, 'first=red');
  Assert.Contains(lResp.Content, 'idsum=7'); // 1+2+4
end;

procedure TTestMinimalWebApi.Test_MapQuery_binds_the_json_body;
var
  lResp: IMVCRESTResponse;
begin
  { QUERY carries a body, so the class argument must be filled from it. If
    httpQUERY were missing from IsHTTPMethodWithBody the handler would receive
    an empty object and the array access would fail. }
  lResp := RESTClient
    .AddHeader('Content-Type', TMVCMediaType.APPLICATION_JSON)
    .Query('/minimal-feat/search', '{"text":"rossi","cities":["rome","milan"]}');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'text=rossi');
  Assert.Contains(lResp.Content, 'cities=2');
  Assert.Contains(lResp.Content, 'first=rome');
end;

procedure TTestMinimalWebApi.Test_MapQuery_route_does_not_answer_GET;
var
  lResp: IMVCRESTResponse;
begin
  { MapQuery registers QUERY only - the verb is not a wildcard. }
  lResp := RESTClient.Get('/minimal-feat/search');
  Assert.AreEqual<Integer>(404, lResp.StatusCode);
end;

procedure TTestMinimalWebApi.Test_Wildcard_segment_captures_rest_of_path;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-feat/files/a/b/c.txt');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'path=a/b/c.txt');
end;

procedure TTestMinimalWebApi.Test_Wildcard_segment_matches_empty_tail;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-feat/files/');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'path=');
end;

procedure TTestMinimalWebApi.Test_RequireRole_unauthenticated_is_401;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient.Get('/minimal-feat/admin');
  Assert.AreEqual<Integer>(401, lResp.StatusCode);
end;

procedure TTestMinimalWebApi.Test_RequireRole_wrong_role_is_403;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient
    .AddHeader('X-Role', 'user')
    .Get('/minimal-feat/admin');
  Assert.AreEqual<Integer>(403, lResp.StatusCode);
end;

procedure TTestMinimalWebApi.Test_RequireRole_correct_role_is_200;
var
  lResp: IMVCRESTResponse;
begin
  lResp := RESTClient
    .AddHeader('X-Role', 'admin')
    .Get('/minimal-feat/admin');
  Assert.AreEqual<Integer>(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'admin-ok');
end;

initialization

TDUnitX.RegisterTestFixture(TTestMinimalWebApi);

end.
