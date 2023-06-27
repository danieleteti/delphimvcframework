// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Filters.StaticFiles;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger, MVCFramework.Commons;

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

  TMVCStaticFileRulesProc = reference to procedure(const Context: TWebContext; var PathInfo: String; var Handled: Boolean);
  TMVCStaticFileMediaTypesCustomizer = reference to procedure(const MediaTypes: TMVCStringDictionary);
  TMVCStaticFilesProtocolFilter = class(TProtocolFilter)
  private
    fSanityCheckOK: Boolean;
    fMediaTypes: TMVCStringDictionary;
    fStaticFilesPath: string;
    fDocumentRoot: string;
    fIndexDocument: string;
    fStaticFilesCharset: string;
    fSPAWebAppSupport: Boolean;
    fRules: TMVCStaticFileRulesProc;
    procedure AddMediaTypes;
    function SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
  protected
    procedure DoFilter(Context: TWebContext); override;
  public
    constructor Create(
      const AStaticFilesPath: string = TMVCStaticFilesDefaults.STATIC_FILES_PATH;
      const ADocumentRoot: string = TMVCStaticFilesDefaults.DOCUMENT_ROOT;
      const AIndexDocument: string = TMVCStaticFilesDefaults.INDEX_DOCUMENT;
      const ASPAWebAppSupport: Boolean = True;
      const AStaticFilesCharset: string = TMVCStaticFilesDefaults.STATIC_FILES_CONTENT_CHARSET;
      const ARules: TMVCStaticFileRulesProc = nil;
      const AMediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer = nil); virtual;
  end;

implementation

uses
  System.SysUtils,
  System.ZLib,
  System.Classes, System.IOUtils;

{ TMVCCompressionProtocolFilter }

procedure TMVCStaticFilesProtocolFilter.AddMediaTypes;
begin
  fMediaTypes.Add('.html', TMVCMediaType.TEXT_HTML);
  fMediaTypes.Add('.htm', TMVCMediaType.TEXT_HTML);
  fMediaTypes.Add('.txt', TMVCMediaType.TEXT_PLAIN);
  fMediaTypes.Add('.text', TMVCMediaType.TEXT_PLAIN);
  fMediaTypes.Add('.csv', TMVCMediaType.TEXT_CSV);
  fMediaTypes.Add('.css', TMVCMediaType.TEXT_CSS);
  fMediaTypes.Add('.js', TMVCMediaType.TEXT_JAVASCRIPT);
  fMediaTypes.Add('.json', TMVCMediaType.APPLICATION_JSON);
  fMediaTypes.Add('.jpg', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.jpeg', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.jpe', TMVCMediaType.IMAGE_JPEG);
  fMediaTypes.Add('.png', TMVCMediaType.IMAGE_PNG);
  fMediaTypes.Add('.ico', TMVCMediaType.IMAGE_X_ICON);
  fMediaTypes.Add('.appcache', TMVCMediaType.TEXT_CACHEMANIFEST);
  fMediaTypes.Add('.svg', TMVCMediaType.IMAGE_SVG_XML);
  fMediaTypes.Add('.xml', TMVCMediaType.TEXT_XML);
  fMediaTypes.Add('.pdf', TMVCMediaType.APPLICATION_PDF);
  fMediaTypes.Add('.svgz', TMVCMediaType.IMAGE_SVG_XML);
  fMediaTypes.Add('.gif', TMVCMediaType.IMAGE_GIF);
end;

constructor TMVCStaticFilesProtocolFilter.Create(const AStaticFilesPath,
  ADocumentRoot, AIndexDocument: string; const ASPAWebAppSupport: Boolean;
  const AStaticFilesCharset: string; const ARules: TMVCStaticFileRulesProc;
  const AMediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer);
begin
  inherited Create;
  fSanityCheckOK := False;
  fStaticFilesPath := AStaticFilesPath.Trim;
//  if not fStaticFilesPath.EndsWith('/') then
//    fStaticFilesPath := fStaticFilesPath + '/';

  if TDirectory.Exists(ADocumentRoot) then
  begin
    fDocumentRoot := TPath.GetFullPath(ADocumentRoot);
  end
  else
  begin
    fDocumentRoot := TPath.Combine(AppPath, ADocumentRoot);
  end;
  fIndexDocument := AIndexDocument;
  fStaticFilesCharset := AStaticFilesCharset;
  fSPAWebAppSupport := ASPAWebAppSupport;
  fMediaTypes := TMVCStringDictionary.Create;
  fRules := ARules;
  AddMediaTypes;
  if Assigned(AMediaTypesCustomizer) then
  begin
    AMediaTypesCustomizer(fMediaTypes);
  end;
end;

procedure TMVCStaticFilesProtocolFilter.DoFilter(Context: TWebContext);
var
  lPathInfo: string;
  lFileName: string;
  lIsDirectoryTraversalAttach: Boolean;
  lFullPathInfo: string;
  lRealFileName: string;
  lAllow: Boolean;
begin
  DoNext(Context);
  if Context.Response.StatusCode <> HTTP_STATUS.NotFound then
  begin
    Exit;
  end;

  lPathInfo := Context.Request.PathInfo;

  if not lPathInfo.StartsWith(fStaticFilesPath, True) then
  begin
    Exit;
//    { In case of folder request without the trailing "/" }
//    if not lPathInfo.EndsWith('/') then
//    begin
//      lPathInfo := lPathInfo + '/';
//      if not lPathInfo.StartsWith(fStaticFilesPath, True) then
//      begin
//        Exit;
//      end;
//    end
//    else
//    begin
//      Exit;
//    end;
  end;

  if Assigned(fRules) then
  begin
    lAllow := True;
    fRules(Context, lPathInfo, lAllow);
    if not lAllow then
    begin
      Exit;
    end;
  end;

  // calculate the actual requested path
  if lPathInfo.StartsWith(fStaticFilesPath, True) then
  begin
    lPathInfo := lPathInfo.Remove(0, fStaticFilesPath.Length);
  end;
  lPathInfo := lPathInfo.Replace('/', PathDelim, [rfReplaceAll]);
  if lPathInfo.StartsWith(PathDelim) then
  begin
    lPathInfo := lPathInfo.Remove(0, 1);
  end;
  lFullPathInfo := TPath.Combine(fDocumentRoot, lPathInfo);

  { Now the actual requested path is in lFullPathInfo }
  if TMVCStaticContents.IsStaticFile(fDocumentRoot, lPathInfo, lRealFileName,
    lIsDirectoryTraversalAttach) then
  begin
    // check if it's a direct file request
    if lIsDirectoryTraversalAttach then
    begin
      Context.Response.StatusCode := HTTP_STATUS.NotFound;
      Exit;
    end;

    if SendStaticFileIfPresent(Context, lRealFileName) then
    begin
      Context.Response.StatusCode := HTTP_STATUS.OK;
      Exit;
    end;
  end;

  // check if a directory request
  if TDirectory.Exists(lFullPathInfo) then
  begin
    if not Context.Request.PathInfo.EndsWith('/') then
    begin
      Context.Response.StatusCode := HTTP_STATUS.MovedPermanently;
      Context.Response.CustomHeaders.Values['Location'] := Context.Request.PathInfo + '/';
      Exit;
    end;

    if not fIndexDocument.IsEmpty then
    begin
      if SendStaticFileIfPresent(Context, TPath.Combine(lFullPathInfo, fIndexDocument)) then
      begin
        Context.Response.StatusCode := HTTP_STATUS.OK;
        Exit;
      end;
    end;
  end;

  // if SPA support is enabled, return the first index.html found in the path.
  // This allows to host multiple SPA application in subfolders
  if fSPAWebAppSupport and (not fIndexDocument.IsEmpty) then
  begin
    while (not lFullPathInfo.IsEmpty) and (not TDirectory.Exists(lFullPathInfo)) do
    begin
      lFullPathInfo := TDirectory.GetParent(lFullPathInfo);
    end;
    lFileName := TPath.GetFullPath(TPath.Combine(lFullPathInfo, fIndexDocument));
    if SendStaticFileIfPresent(Context, lFileName) then
    begin
      Exit;
    end;
  end;
end;

function TMVCStaticFilesProtocolFilter.SendStaticFileIfPresent(
  const AContext: TWebContext; const AFileName: string): Boolean;
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
    Log(TLogLevel.levDebug, AContext.Request.HTTPMethodAsString + ':' +
      AContext.Request.PathInfo + ' [' + AContext.Request.ClientIp + '] -> ' +
      ClassName + ' - ' + IntToStr(AContext.Response.StatusCode) + ' ' +
      AContext.Response.ReasonString);
  end;
end;

end.
