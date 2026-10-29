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

unit HTTPFilterSupportU;

// The server side of the HTTP filter tests (HTTPFilterTestsU in TestClient): the ready-made filters of
// MVCFramework.Filters, each on a prefix of its own, with a few controllers. Shared by the TestServer, which
// every host runs (WebBroker, Indy Direct, HTTP.sys, ISAPI, Apache), and by the in-process servers of the
// tests. The filters act only on their own prefix; every other request goes through them untouched.
//
//   /fstatic  StaticFilesHTTPFilter(prefix, folder)           (the classic middleware does the same at /static)
//   /fspa     StaticFilesHTTPFilter(prefix, folder, sfmSPA)   (the classic middleware does the same at /spa)
//   /fzip     CompressionHTTPFilter + StaticFilesHTTPFilter   /fetag  ETagHTTPFilter + StaticFilesHTTPFilter
//   /fmedia   RangeMedia, the folder written with /
//   /ffiles   FileServerBundle(prefix, folder)          /fapp  FileServerBundle(prefix, folder, fsmSPA) + an API
//   /fstrict  FileServerBundle(options): case-sensitive prefix, a rule that refuses folder1.html
//   /fexc     CORS + a controller that raises + a filter that raises
//   /fold, /fold2  Redirect (permanent, temporary)
//   /ftrace   Trace + a controller

interface

uses
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons;

type
  // the routes of the application, under the prefix that the SPA filter answers
  [MVCPath('/fspa/api')]
  TSPAApiController = class(TMVCController)
  public
    [MVCPath('/ping')]
    [MVCHTTPMethod([httpGET])]
    function Ping: String;
    // a 404 that the action chose: it must not become index.html
    [MVCPath('/gone')]
    [MVCHTTPMethod([httpGET])]
    function Gone: IMVCResponse;
  end;

  // the API next to the SPA of FileServerBundle, on its prefix
  [MVCPath('/fapp/api')]
  TFileServerApiController = class(TMVCController)
  public
    [MVCPath('/info')]
    [MVCHTTPMethod([httpGET])]
    function Info: TJsonObject;
  end;

  // the answer of a controller (not a file): what Compression and ETag work on in an application
  [MVCPath('/fzip/api')]
  TFilterZipApiController = class(TMVCController)
  public
    [MVCPath('/big')]
    [MVCHTTPMethod([httpGET])]
    function Big: String;
  end;

  [MVCPath('/fetag/api')]
  TFilterETagApiController = class(TMVCController)
  public
    [MVCPath('/big')]
    [MVCHTTPMethod([httpGET])]
    function Big: String;
  end;

  [MVCPath('/fexc')]
  TFilterExceptionsController = class(TMVCController)
  public
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    function Boom: String;
  end;

  [MVCPath('/ftrace')]
  TFilterTraceController = class(TMVCController)
  public
    [MVCPath('/echo')]
    [MVCHTTPMethod([httpGET])]
    function Echo: String;
  end;

// a filter that acts on the requests of a path prefix only and lets every other one through
function OnlyFor(const APrefix: string; const AFilter: TMVCHTTPFilter): TMVCHTTPFilter;

// AWwwRoot is the folder served at /fstatic, /fspa, /fzip, /fetag, /ffiles, /fapp and /fstrict; its sub folder
// media is served at /fmedia.
// The TestServer passes 'www' (the folder the classic middleware serves), the in-process servers an absolute one.
procedure ConfigureHTTPFilterSupport(const AEngine: TMVCEngine; const AWwwRoot: string);

implementation

uses
  System.SysUtils,
  MVCFramework.Filters;

{ TSPAApiController }

function TSPAApiController.Ping: String;
begin
  Result := 'pong';
end;

function TSPAApiController.Gone: IMVCResponse;
begin
  Result := NotFoundResponse('gone');
end;

{ TFileServerApiController }

function TFileServerApiController.Info: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S['from'] := 'api';
end;

function BigText: String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to 220 do
    Result := Result + 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ';
end;

{ TFilterZipApiController }

function TFilterZipApiController.Big: String;
begin
  Result := BigText;
end;

{ TFilterETagApiController }

function TFilterETagApiController.Big: String;
begin
  Result := BigText;
end;

{ TFilterExceptionsController }

function TFilterExceptionsController.Boom: String;
begin
  raise Exception.Create('boom');
end;

{ TFilterTraceController }

function TFilterTraceController.Echo: String;
begin
  Result := 'echoed';
end;

function OnlyFor(const APrefix: string; const AFilter: TMVCHTTPFilter): TMVCHTTPFilter;
begin
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if AContext.Request.PathInfo.StartsWith(APrefix, True) then
        AFilter(AContext, ANext)
      else
        ANext();
    end;
end;

procedure ConfigureHTTPFilterSupport(const AEngine: TMVCEngine; const AWwwRoot: string);
var
  lFileServer: TMVCFileServerOptions;
begin
  AEngine
    .AddController(TSPAApiController)
    .AddController(TFileServerApiController)
    .AddController(TFilterZipApiController)
    .AddController(TFilterETagApiController)
    .AddController(TFilterExceptionsController)
    .AddController(TFilterTraceController);

  AEngine.Use(StaticFilesHTTPFilter('/fstatic', AWwwRoot));
  AEngine.Use(StaticFilesHTTPFilter('/fspa', AWwwRoot, sfmSPA));

  // the filters come before the files they work on: the first registered is the outermost
  AEngine.Use(OnlyFor('/fzip', CompressionHTTPFilter(256)));
  AEngine.Use(StaticFilesHTTPFilter('/fzip', AWwwRoot));
  AEngine.Use(OnlyFor('/fetag', ETagHTTPFilter()));
  AEngine.Use(StaticFilesHTTPFilter('/fetag', AWwwRoot));

  // the folder written with / : on Windows it is a mix of separators once it is joined to AppPath
  AEngine.Use(RangeMediaHTTPFilter('/fmedia', StringReplace(AWwwRoot, PathDelim, '/', [rfReplaceAll]) + '/media'));

  AEngine.Use(FileServerBundle('/ffiles', AWwwRoot));
  AEngine.Use(FileServerBundle('/fapp', AWwwRoot, fsmSPA));
  lFileServer := TMVCFileServerOptions.Default;
  lFileServer.URLPrefix := '/fstrict';
  lFileServer.DocumentRoot := AWwwRoot;
  lFileServer.Flags := [fsfRanges, fsfCaseSensitivePrefix];
  lFileServer.Rules :=
    procedure(const AContext: TWebContext; var APathInfo: string; var AAllow: Boolean)
    begin
      if APathInfo.EndsWith('/folder1.html') then
      begin
        AAllow := False;
        AContext.Response.StatusCode := http_status.Forbidden;
      end;
    end;
  AEngine.Use(FileServerBundle(lFileServer));

  AEngine.Use(OnlyFor('/fexc', CORSHTTPFilter('*')));
  AEngine.Use(OnlyFor('/fexc/filter-raises',
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      raise EMVCException.Create(http_status.Forbidden, 'no way');
    end));

  AEngine.Use(RedirectHTTPFilter(['/fold'], '/fnew', rdkPermanent));
  AEngine.Use(RedirectHTTPFilter(['/fold2'], '/fnew'));

  AEngine.Use(OnlyFor('/ftrace', TraceHTTPFilter(1024, 'trace_test')));
end;

end.
