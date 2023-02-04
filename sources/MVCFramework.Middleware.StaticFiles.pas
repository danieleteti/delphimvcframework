// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

  TMVCStaticFileRulesProc = reference to procedure(const Context: TWebContext; var PathInfo: String; var Handled: Boolean);
  TMVCStaticFileMediaTypesCustomizer = reference to procedure(const MediaTypes: TMVCStringDictionary);
  TMVCStaticFilesMiddleware = class(TInterfacedObject, IMVCMiddleware)
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
    // function IsStaticFileRequest(const APathInfo: string; out AFileName: string;
    // out AIsDirectoryTraversalAttach: Boolean): Boolean;
    function SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
    procedure DoSanityCheck;
  public
    constructor Create(
      const AStaticFilesPath: string = TMVCStaticFilesDefaults.STATIC_FILES_PATH;
      const ADocumentRoot: string = TMVCStaticFilesDefaults.DOCUMENT_ROOT;
      const AIndexDocument: string = TMVCStaticFilesDefaults.INDEX_DOCUMENT;
      const ASPAWebAppSupport: Boolean = True;
      const AStaticFilesCharset: string = TMVCStaticFilesDefaults.STATIC_FILES_CONTENT_CHARSET;
      const ARules: TMVCStaticFileRulesProc = nil;
      const AMediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer = nil);
    destructor Destroy; override;

    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);

    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);

    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  MVCFramework.Logger,
  System.SysUtils,
  System.NetEncoding,
  System.IOUtils,
  System.Classes;

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

constructor TMVCStaticFilesMiddleware.Create(
      const AStaticFilesPath: string;
      const ADocumentRoot: string;
      const AIndexDocument: string;
      const ASPAWebAppSupport: Boolean;
      const AStaticFilesCharset: string;
      const ARules: TMVCStaticFileRulesProc;
      const AMediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer);
begin
  inherited Create;
  fSanityCheckOK := False;
  fStaticFilesPath := AStaticFilesPath.Trim;
  if not fStaticFilesPath.EndsWith('/') then
    fStaticFilesPath := fStaticFilesPath + '/';

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

destructor TMVCStaticFilesMiddleware.Destroy;
begin
  fMediaTypes.Free;

  inherited Destroy;
end;

procedure TMVCStaticFilesMiddleware.DoSanityCheck;
begin
  if not fStaticFilesPath.StartsWith('/') then
  begin
    raise EMVCException.Create('StaticFilePath must begin with "/" and cannot be empty');
  end;
  if not TDirectory.Exists(fDocumentRoot) then
  begin
    raise EMVCException.CreateFmt('TMVCStaticFilesMiddleware Error: DocumentRoot [%s] is not a valid directory', [fDocumentRoot]);
  end;
  fSanityCheckOK := True;
end;

// function TMVCStaticFilesMiddleware.IsStaticFileRequest(const APathInfo: string; out AFileName: string;
// out AIsDirectoryTraversalAttach: Boolean): Boolean;
// begin
// Result := TMVCStaticContents.IsStaticFile(fDocumentRoot, APathInfo, AFileName,
// AIsDirectoryTraversalAttach);
// end;

procedure TMVCStaticFilesMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
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
  lIsDirectoryTraversalAttach: Boolean;
  lFullPathInfo: string;
  lRealFileName: string;
  lAllow: Boolean;
begin
//  if not fSanityCheckOK then
//  begin
//    DoSanityCheck;
//  end;

  lPathInfo := AContext.Request.PathInfo;

  if not lPathInfo.StartsWith(fStaticFilesPath, True) then
  begin
    { In case of folder request without the trailing "/" }
    if not lPathInfo.EndsWith('/') then
    begin
      lPathInfo := lPathInfo + '/';
      if not lPathInfo.StartsWith(fStaticFilesPath, True) then
      begin
        AHandled := False;
        Exit;
      end;
    end
    else
    begin
      AHandled := False;
      Exit;
    end;
  end;

  if Assigned(fRules) then
  begin
    lAllow := True;
    fRules(AContext, lPathInfo, lAllow);
    if not lAllow then
    begin
      AHandled := True;
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

  if not fSanityCheckOK then
  begin
    DoSanityCheck;
  end;

  if TMVCStaticContents.IsStaticFile(fDocumentRoot, lPathInfo, lRealFileName,
    lIsDirectoryTraversalAttach) then
  begin
    // check if it's a direct file request
    // lIsFileRequest := TMVCStaticContents.IsStaticFile(fDocumentRoot, lPathInfo, lRealFileName,
    // lIsDirectoryTraversalAttach);
    if lIsDirectoryTraversalAttach then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.NotFound;
      AHandled := True;
      Exit;
    end;

    AHandled := SendStaticFileIfPresent(AContext, lRealFileName);
    if AHandled then
    begin
      Exit;
    end;
  end;

  // check if a directory request
  if TDirectory.Exists(lFullPathInfo) then
  begin
    if not AContext.Request.PathInfo.EndsWith('/') then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.MovedPermanently;
      AContext.Response.CustomHeaders.Values['Location'] := AContext.Request.PathInfo + '/';
      AHandled := True;
      Exit;
    end;

    if not fIndexDocument.IsEmpty then
    begin
      AHandled := SendStaticFileIfPresent(AContext, TPath.Combine(lFullPathInfo, fIndexDocument));
      Exit;
    end;
  end;

  // if SPA support is enabled, return the first index.html found in the path.
  // This allows to host multiple SPA application in subfolders
  if (not AHandled) and fSPAWebAppSupport and (not fIndexDocument.IsEmpty) then
  begin
    while (not lFullPathInfo.IsEmpty) and (not TDirectory.Exists(lFullPathInfo)) do
    begin
      lFullPathInfo := TDirectory.GetParent(lFullPathInfo);
    end;
    lFileName := TPath.GetFullPath(TPath.Combine(lFullPathInfo, fIndexDocument));
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
    Log(TLogLevel.levDebug, AContext.Request.HTTPMethodAsString + ':' +
      AContext.Request.PathInfo + ' [' + AContext.Request.ClientIp + '] -> ' +
      ClassName + ' - ' + IntToStr(AContext.Response.StatusCode) + ' ' +
      AContext.Response.ReasonString);
  end;
end;

end.
