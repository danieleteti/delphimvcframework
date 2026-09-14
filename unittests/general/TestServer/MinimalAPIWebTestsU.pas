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

unit MinimalAPIWebTestsU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI;

type
  // QUERY: a class argument binds the JSON body. Declared here in the interface
  // section on purpose - the minimal dispatcher instantiates a bound class by
  // its qualified name, which RTTI only resolves for interface-section types.
  // The criteria carry an array, the shape that made QUERY worth having.
  TQuerySearch = class
  private
    FText: string;
    FCities: TArray<string>;
  public
    property Text: string read FText write FText;
    property Cities: TArray<string> read FCities write FCities;
  end;

procedure RegisterMinimalAPIWebRoutes(AEngine: TMVCEngine);

implementation

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.Validators,
  MVCFramework.Filters;

type
  TLoginForm = record
    [MVCFromContentField('username')] Username: string;
    [MVCFromContentField('password')] Password: string;
    [MVCFromContentField('remember', 'false')] Remember: Boolean;
  end;

  TMultiValueForm = record
    [MVCFromContentField('tag')] Tags: TArray<string>;
  end;

  // #2 record validation: MinLength(3) on a [MVCFromQueryString] field
  TValidatedQuery = record
    [MVCFromQueryString('q')]
    [MVCMinLength(3)]
    Q: string;
  end;

  // #3 file upload: record mixing a [MVCFromFile] field with a text field
  TUploadForm = record
    [MVCFromContentField('title')] Title: string;
    [MVCFromFile('doc')] Doc: TMVCFormFile;
  end;

  // #4 typed array binding from repeated query keys
  TTagQuery = record
    [MVCFromQueryString('tag')] Tags: TArray<string>;
    [MVCFromQueryString('id')]  Ids: TArray<Integer>;
  end;

// #6 test-only authentication filter: promotes the X-Role header to LoggedUser.
function FakeAuthFromHeader: TMVCEndpointFilter;
begin
  Result :=
    function (const Ctx: TWebContext;
              const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      lRole: string;
    begin
      lRole := Ctx.Request.Headers['X-Role'];
      if lRole <> '' then
      begin
        Ctx.LoggedUser.UserName := 'tester';
        Ctx.LoggedUser.LoggedSince := Now; // IsValid requires UserName + LoggedSince
        Ctx.LoggedUser.Roles.Clear;
        Ctx.LoggedUser.Roles.Add(lRole);
      end;
      Result := Next();
    end;
end;

procedure RegisterMinimalAPIWebRoutes(AEngine: TMVCEngine);
begin
  // -- routing test: simple .AsWeb GET returns text/html
  AEngine.Root.AsWeb.MapGet('/minimal-web/hello',
    function: IMVCResponse
    begin
      ViewData['who'] := 'world';
      Result := RenderView('minimal_web_hello');
    end);

  // -- ViewData test: round-trip a few keys via Ctx.ViewData
  AEngine.Root.AsWeb.MapGet<TWebContext>('/minimal-web/viewdata',
    function (Ctx: TWebContext): IMVCResponse
    begin
      Ctx.ViewData['a'] := 'alpha';
      Ctx.ViewData['b'] := 'beta';
      Result := RenderView('minimal_web_viewdata');
    end);

  // -- form binding test
  AEngine.Root.AsWeb.MapPost<TLoginForm>('/minimal-web/login',
    function (F: TLoginForm): IMVCResponse
    begin
      ViewData['username'] := F.Username;
      ViewData['remember'] := F.Remember;
      Result := RenderView('minimal_web_login_result');
    end);

  // -- multi-value form binding: [MVCFromContentField] on TArray<string>
  AEngine.Root.AsWeb.MapPost<TMultiValueForm>('/minimal-web/multi',
    function (F: TMultiValueForm): IMVCResponse
    begin
      ViewData['count'] := IntToStr(Length(F.Tags));
      if Length(F.Tags) >= 1 then
        ViewData['first'] := F.Tags[0]
      else
        ViewData['first'] := '';
      if Length(F.Tags) >= 2 then
        ViewData['second'] := F.Tags[1]
      else
        ViewData['second'] := '';
      Result := RenderView('minimal_web_multi');
    end);

  // -- filter renders error page
  AEngine.Root.AsWeb
    .Use(
      function (const Ctx: TWebContext;
        const Next: TMVCEndpointFilterNext): IMVCResponse
      begin
        if Ctx.Request.Params['block'] = '1' then
          Result := RenderView('minimal_web_blocked')
        else
          Result := Next();
      end)
    .MapGet('/minimal-web/filter',
      function: IMVCResponse
      begin
        Result := RenderView('minimal_web_filter_ok');
      end);

  // -- OpenAPI: API + Web siblings, plus a web route opted in
  AEngine.Root.MapGet('/minimal-web/api-side',
    function: IMVCResponse
    begin
      Result := Ok('api');
    end);
  AEngine.Root.AsWeb.MapGet('/minimal-web/web-side',
    function: IMVCResponse
    begin
      Result := RenderView('minimal_web_side');
    end);
  AEngine.Root.AsWeb.MapGet('/minimal-web/web-visible',
    function: IMVCResponse
    begin
      Result := RenderView('minimal_web_side');
    end).WithOpenAPI(True);

  // -- threadvar isolation: parameterised endpoint that echoes a marker
  AEngine.Root.AsWeb.MapGet<string>('/minimal-web/iso/($marker)',
    function (Marker: string): IMVCResponse
    begin
      ViewData['marker'] := Marker;
      Result := RenderView('minimal_web_iso');
    end);

  // -- content negotiation: same VERB + PATH, two RouteKinds. The dispatcher
  // picks rkApi when Accept says application/json, rkWeb when text/html.
  AEngine.Root.MapGet('/minimal-web/negotiate',
    function: IMVCResponse
    begin
      Result := Ok('negotiate-api');
    end);
  AEngine.Root.AsWeb.MapGet('/minimal-web/negotiate',
    function: IMVCResponse
    begin
      Result := RenderView('minimal_web_negotiate');
    end);

  // ===== Minimal API parity features ======================================

  // #2 record validation: MinLength(3) on a [MVCFromQueryString] field
  AEngine.Root.MapGet<TValidatedQuery>('/minimal-feat/validate',
    function (Q: TValidatedQuery): IMVCResponse
    begin
      Result := Ok('q=' + Q.Q);
    end);

  // #3 single TMVCFormFile arg -> first uploaded file
  AEngine.Root.MapPost<TMVCFormFile>('/minimal-feat/upload1',
    function (F: TMVCFormFile): IMVCResponse
    begin
      if F = nil then
        Result := BadRequest('no file')
      else
        Result := Ok(Format('name=%s size=%d', [F.FileName, F.Size]));
    end);

  // #3 record with a [MVCFromFile] field alongside a text field
  AEngine.Root.MapPost<TUploadForm>('/minimal-feat/upload',
    function (F: TUploadForm): IMVCResponse
    var
      lFileName: string;
    begin
      if F.Doc = nil then
        lFileName := '<none>'
      else
        lFileName := F.Doc.FileName;
      Result := Ok(Format('title=%s file=%s', [F.Title, lFileName]));
    end);

  // QUERY via the dedicated MapQuery helper: proves the verb reaches the
  // minimal dispatcher AND that the body is bound (httpQUERY has to be in
  // IsHTTPMethodWithBody for the class argument to be filled).
  AEngine.Root.MapQuery<TQuerySearch>('/minimal-feat/search',
    function (Q: TQuerySearch): IMVCResponse
    begin
      Result := Ok(Format('text=%s cities=%d first=%s',
        [Q.Text, Length(Q.Cities), Q.Cities[0]]));
    end);

  // #4 typed array binding from repeated query-string keys
  AEngine.Root.MapGet<TTagQuery>('/minimal-feat/tags',
    function (Q: TTagQuery): IMVCResponse
    var
      lSum, I: Integer;
      lFirst: string;
    begin
      lSum := 0;
      for I := 0 to High(Q.Ids) do
        lSum := lSum + Q.Ids[I];
      if Length(Q.Tags) > 0 then
        lFirst := Q.Tags[0]
      else
        lFirst := '';
      Result := Ok(Format('tags=%d first=%s idsum=%d',
        [Length(Q.Tags), lFirst, lSum]));
    end);

  // #5 trailing wildcard segment ($path:*) captures the rest of the URL
  AEngine.Root.MapGet<string>('/minimal-feat/files/($path:*)',
    function (Path: string): IMVCResponse
    begin
      Result := Ok('path=' + Path);
    end);

  // #6 declarative authorization: FakeAuth populates LoggedUser, RequireRole gates
  AEngine.Root
    .Use(FakeAuthFromHeader())
    .Use(RequireRole('admin'))
    .MapGet('/minimal-feat/admin',
      function: IMVCResponse
      begin
        Result := Ok('admin-ok');
      end);
end;

end.
