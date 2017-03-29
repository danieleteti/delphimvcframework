// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit MVCFramework.View;

{$I dmvcframework.inc}

interface

{$WARNINGS OFF}

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Data.DB,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.TypesAliases,
  SynMustache,
  SynCommons;

{$WARNINGS ON}

type

  TMVCBaseView = class(TMVCBase)
  private
    FViewName: string;
    FWebContext: TWebContext;
    FViewModel: TMVCViewDataObject;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FContentType: string;
    FOutput: string;
  protected
    function GetRealFileName(const AViewName: string): string; virtual;
    function IsCompiledVersionUpToDate(const AFileName, ACompiledFileName: string): Boolean; virtual; abstract;
  public
    constructor Create(
      const AViewName: string;
      const AEngine: TMVCEngine;
      const AWebContext: TWebContext;
      const AViewModel: TMVCViewDataObject;
      const AViewDataSets: TObjectDictionary<string, TDataSet>;
      const AContentType: string); virtual;
    destructor Destroy; override;

    procedure Execute; virtual; abstract;

    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;
    property ViewModel: TMVCViewDataObject read FViewModel;
    property ViewDataSets: TObjectDictionary<string, TDataSet> read FViewDataSets;
    property ContentType: string read FContentType;
    property Output: string read FOutput;
  end;

  TMVCMustacheView = class(TMVCBaseView)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    procedure Execute; override;
  end;

implementation

{ TMVCBaseView }

constructor TMVCBaseView.Create(
  const AViewName: string;
  const AEngine: TMVCEngine;
  const AWebContext: TWebContext;
  const AViewModel: TMVCViewDataObject;
  const AViewDataSets: TObjectDictionary<string, TDataSet>;
  const AContentType: string);
begin
  inherited Create;
  FViewName := AViewName;
  Engine := AEngine;
  FWebContext := AWebContext;
  FViewModel := AViewModel;
  FViewDataSets := AViewDataSets;
  FContentType := AContentType;
  FOutput := EmptyStr;
end;

destructor TMVCBaseView.Destroy;
begin
  inherited Destroy;
end;

function TMVCBaseView.GetRealFileName(const AViewName: string): string;
var
  FileName: string;
  F: string;
  DefaultViewFileExtension: string;
begin
  DefaultViewFileExtension := Config[TMVCConfigKey.DefaultViewFileExtension];
  FileName := stringReplace(AViewName, '/', '\', [rfReplaceAll]);

  if (FileName = '\') then
    FileName := '\index.' + DefaultViewFileExtension
  else
    FileName := FileName + '.' + DefaultViewFileExtension;

  if DirectoryExists(Config[TMVCConfigKey.ViewPath]) then
    F := ExpandFileName(IncludeTrailingPathDelimiter(Config.Value[TMVCConfigKey.ViewPath]) + FileName)
  else
    F := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath + Config.Value[TMVCConfigKey.ViewPath]) + FileName);

  if not TFile.Exists(F) then
    FileName := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath + Config.Value[TMVCConfigKey.DocumentRoot]) + FileName)
  else
    FileName := F;

  if FileExists(FileName) then
    Result := FileName
  else
    Result := EmptyStr;
end;

{ TMVCMustacheView }

{$WARNINGS OFF}

procedure TMVCMustacheView.Execute;
var
  ViewFileName: string;
  ViewTemplate: RawUTF8;
  ViewEngine: TSynMustache;
  DataObj: TPair<string, string>;
  Jo: TJSONObject;
  lSJSON: String;
  lFirst: Boolean;
begin
  ViewFileName := GetRealFileName(ViewName);
  if not FileExists(ViewFileName) then
    raise EMVCFrameworkViewException.CreateFmt('View [%s] not found', [ViewName]);

  ViewTemplate := StringToUTF8(TFile.ReadAllText(ViewFileName, TEncoding.UTF8));
  ViewEngine := TSynMustache.Parse(ViewTemplate);
  lSJSON := '{';
  if Assigned(FViewModel) then
  begin
    lFirst := True;
    for DataObj in FViewModel do
    begin
      if not lFirst then
        lSJSON := lSJSON + ',';
      lSJSON := lSJSON + '"' + DataObj.Key + '":' + DataObj.Value;
      lFirst := False;
    end;
    lSJSON := lSJSON + '}';
  end;
  FOutput := UTF8Tostring(ViewEngine.RenderJSON(lSJSON));
end;

{$WARNINGS ON}

end.
