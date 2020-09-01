// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// João Antônio Duarte (https://github.com/joaoduarte19)
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

unit MVCFramework.Middleware.StaticFiles;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections;

type
  TMVCStaticFilesDefaults = class sealed
  public const
    /// <summary>
    /// URL segment that represents the path to static files
    /// </summary>
    STATIC_FILES_PATH = '/static';

    /// <summary>
    /// Physical path of the root folder that contains the static files
    /// </summary>
    DOCUMENT_ROOT = '.\www';

    /// <summary>
    /// Default static file
    /// </summary>
    INDEX_DOCUMENT = 'index.html';

    /// <summary>
    /// Charset of static files
    /// </summary>
    STATIC_FILES_CONTENT_CHARSET = TMVCConstants.DEFAULT_CONTENT_CHARSET;
  end;

  TMVCStaticFilesMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fMediaTypes: TDictionary<string, string>;
    fStaticFilesPath: string;
    fDocumentRoot: string;
    fIndexDocument: string;
    fStaticFilesCharset: string;
    fSPAWebAppSupport: Boolean;
    procedure AddMediaTypes;
    function IsStaticFileRequest(const APathInfo: string; out AFileName: string): Boolean;
    function SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
  public
    constructor Create(
      const AStaticFilesPath: string = TMVCStaticFilesDefaults.STATIC_FILES_PATH;
      const ADocumentRoot: string = TMVCStaticFilesDefaults.DOCUMENT_ROOT;
      const AIndexDocument: string = TMVCStaticFilesDefaults.INDEX_DOCUMENT;
      const ASPAWebAppSupport: Boolean = True;
      const AStaticFilesCharset: string = TMVCStaticFilesDefaults.STATIC_FILES_CONTENT_CHARSET);
    destructor Destroy; override;

    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);

    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);

    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  System.SysUtils,
  System.NetEncoding,
  System.IOUtils, System.Classes;

{ TMVCStaticFilesMiddleware }

procedure TMVCStaticFilesMiddleware.AddMediaTypes;
begin
  fMediaTypes.Add('.html', TMVCMediaType.TEXT_HTML);
  fMediaTypes.Add('.htm', TMVCMediaType.TEXT_HTML);
  fMediaTypes.Add('.txt', TMVCMediaType.TEXT_PLAIN);
  fMediaTypes.Add('.text', TMVCMediaType.TEXT_PLAIN);
  fMediaTypes.Add('.csv', TMVCMediaType.TEXT_CSV);
  fMediaTypes.Add('.css', TMVCMediaType.TEXT_CSS);
  fMediaTypes.Add('.js', TMVCMediaType.TEXT_JAVASCRIPT);
  fMediaTypes.Add('.jpg', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.jpeg', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.jpe', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.png', TMVCMediaType.IMAGE_PNG);
  fMediaTypes.Add('.ico', TMVCMediaType.IMAGE_X_ICON);
  fMediaTypes.Add('.appcache', TMVCMediaType.TEXT_CACHEMANIFEST);
  fMediaTypes.Add('.svg', TMVCMediaType.IMAGE_SVG_XML);
  fMediaTypes.Add('.svgz', TMVCMediaType.IMAGE_SVG_XML);
  fMediaTypes.Add('.gif', TMVCMediaType.IMAGE_GIF);
end;

constructor TMVCStaticFilesMiddleware.Create(
  const AStaticFilesPath: string = TMVCStaticFilesDefaults.STATIC_FILES_PATH;
  const ADocumentRoot: string = TMVCStaticFilesDefaults.DOCUMENT_ROOT;
  const AIndexDocument: string = TMVCStaticFilesDefaults.INDEX_DOCUMENT;
  const ASPAWebAppSupport: Boolean = True;
  const AStaticFilesCharset: string = TMVCStaticFilesDefaults.STATIC_FILES_CONTENT_CHARSET);
begin
  inherited Create;
  fStaticFilesPath := AStaticFilesPath;
  fDocumentRoot := TPath.Combine(AppPath, ADocumentRoot);
  fIndexDocument := AIndexDocument;
  fStaticFilesCharset := AStaticFilesCharset;
  fSPAWebAppSupport := ASPAWebAppSupport;
  fMediaTypes := TDictionary<string, string>.Create;
  AddMediaTypes;
end;

destructor TMVCStaticFilesMiddleware.Destroy;
begin
  fMediaTypes.Free;

  inherited Destroy;
end;

function TMVCStaticFilesMiddleware.IsStaticFileRequest(const APathInfo: string; out AFileName: string): Boolean;
begin
  Result := (not fDocumentRoot.IsEmpty) and (TMVCStaticContents.IsStaticFile(fDocumentRoot, APathInfo, AFileName));
end;

procedure TMVCStaticFilesMiddleware.OnAfterControllerAction(AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCStaticFilesMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCStaticFilesMiddleware.OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCStaticFilesMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lPathInfo: string;
  lFileName: string;
begin
  lPathInfo := AContext.Request.PathInfo;

  if not lPathInfo.StartsWith(fStaticFilesPath, True) then
  begin
    AHandled := False;
    Exit;
  end;

  {
    If user ask for
    www.server.it/folder
    the browser is redirected to
    www.server.it/folder/
  }
  if SameText(lPathInfo, fStaticFilesPath) then
  begin
    if (not lPathInfo.EndsWith('/')) and (not fIndexDocument.IsEmpty) then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.MovedPermanently;
      AContext.Response.CustomHeaders.Values['Location'] := lPathInfo + '/';
      AHandled := True;
      Exit;
    end;
  end;

  if not((fStaticFilesPath = '/') or (fStaticFilesPath = '')) then
  begin
    lPathInfo := lPathInfo.Remove(0, fStaticFilesPath.Length);
  end;

  if not fIndexDocument.IsEmpty then
  begin
    if (lPathInfo = '/') or (lPathInfo = '') then
    begin
      lFileName := TPath.GetFullPath(TPath.Combine(fDocumentRoot, fIndexDocument));
      AHandled := SendStaticFileIfPresent(AContext, lFileName);
    end;
  end;

  if (not AHandled) and (IsStaticFileRequest(lPathInfo, lFileName)) then
  begin
    AHandled := SendStaticFileIfPresent(AContext, lFileName);
  end;

  // if (not AHandled) and lPathInfo.EndsWith('favicon.ico') then
  // begin
  // AContext.Response.SetContentStream(TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(DMVC_FAVICON)),
  // TMVCMediaType.IMAGE_X_ICON);
  // AHandled := True;
  // end;

  if (not AHandled) and fSPAWebAppSupport and AContext.Request.ClientPreferHTML and (not fIndexDocument.IsEmpty) then
  begin
    lFileName := TPath.GetFullPath(TPath.Combine(fDocumentRoot, fIndexDocument));
    AHandled := SendStaticFileIfPresent(AContext, lFileName);
  end;
end;

function TMVCStaticFilesMiddleware.SendStaticFileIfPresent(const AContext: TWebContext;
  const AFileName: string): Boolean;
var
  lContentType: string;
begin
  Result := False;
  if TFile.Exists(AFileName) then
  begin
    if fMediaTypes.TryGetValue(LowerCase(ExtractFileExt(AFileName)), lContentType) then
    begin
      lContentType := BuildContentType(lContentType, fStaticFilesCharset);
    end
    else
    begin
      lContentType := BuildContentType(TMVCMediaType.APPLICATION_OCTETSTREAM, '');
    end;
    TMVCStaticContents.SendFile(AFileName, lContentType, AContext);
    Result := True;
  end;
end;

end.
