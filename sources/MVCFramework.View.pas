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

{$WARNINGS OFF}

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections,
  Data.DB,
  MVCFramework.View.Cache,
  System.SysUtils,
  SynMustache,
  SynCommons,
  MVCFramework.Patches;

type
  TMVCBaseView = class(TMVCBase)
  private
    FViewName: string;
    FWebContext: TWebContext;
    FViewModel: TMVCDataObjects;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FMVCEngine: TMVCEngine;
    // FViewCache   : TViewCache;
    // procedure SetViewCache(const Value: TViewCache);

  strict protected
    FCurrentContentType: string;
    FOutput: string;

  protected
    /// <summary>
    /// returns the real name of the file or empty string if no
    // suitable file is found
    /// </summary>
    function GetRealFileName(AViewName: String): String; virtual;
    function IsCompiledVersionUpToDate(const FileName, CompiledFileName: string)
      : Boolean; virtual; abstract;
    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;

  public
    constructor Create(AViewName: string; AMVCEngine: TMVCEngine;
      AWebContext: TWebContext; AViewModels: TMVCDataObjects;
      AViewDataSets: TObjectDictionary<string, TDataSet>;
      ACurrentContentType: string); virtual;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    function GetOutput: String;
    // property ViewCache: TViewCache read FViewCache write SetViewCache;
  end;

  TMVCMustacheView = class(TMVCBaseView)
  public
    procedure Execute; override;
  end;

implementation

uses
  System.IOUtils
    , System.Classes
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF};

{ TMVCBaseView }

constructor TMVCBaseView.Create(AViewName: string; AMVCEngine: TMVCEngine;
  AWebContext: TWebContext; AViewModels: TMVCDataObjects;
  AViewDataSets: TObjectDictionary<string, TDataSet>;
  ACurrentContentType: string);
begin
  inherited Create;
  FViewName := AViewName;
  FWebContext := AWebContext;
  FMVCEngine := AMVCEngine;
  FViewModel := AViewModels;
  FViewDataSets := AViewDataSets;
  FCurrentContentType := ACurrentContentType;
end;

destructor TMVCBaseView.Destroy;
begin
  inherited;
end;

function TMVCBaseView.GetOutput: String;
begin
  Result := FOutput;
end;

function TMVCBaseView.GetRealFileName(AViewName: String): String;
var
  LFileName: string;
  _FFileName: string;
  LDefaultViewFileExtension: string;
begin
  LDefaultViewFileExtension := GetMVCConfig
    [TMVCConfigKey.DefaultViewFileExtension];
  LFileName := StringReplace(AViewName, '/', '\', [rfReplaceAll]);
  // $0.02 of normalization
  if LFileName = '\' then
    LFileName := '\index.' + LDefaultViewFileExtension
  else
    LFileName := LFileName + '.' + LDefaultViewFileExtension;

  if DirectoryExists(GetMVCConfig[TMVCConfigKey.ViewPath]) then
    _FFileName := ExpandFileName
      (IncludeTrailingPathDelimiter(GetMVCConfig.Value[TMVCConfigKey.ViewPath])
      + LFileName)
  else
    _FFileName := ExpandFileName
      (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
      GetMVCConfig.Value[TMVCConfigKey.ViewPath]) + LFileName);

  // if not found in the view_path folder, look in the document_root
  if not TFile.Exists(_FFileName) then
    LFileName := ExpandFileName
      (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
      GetMVCConfig.Value[TMVCConfigKey.DocumentRoot]) + LFileName)
  else
    LFileName := _FFileName;

  if FileExists(LFileName) then
  begin
    Result := LFileName;
  end
  else
  begin
    Result := '';
  end;
end;

// procedure TMVCBaseView.SetViewCache(const Value: TViewCache);
// begin
// FViewCache := Value;
// end;

{ TMVCMustacheView }

procedure TMVCMustacheView.Execute;
var
  LFileName: String;
  LTemplate: RawUTF8;
  LMEngine: TSynMustache;
  LPair: TPair<String, TJSONValue>;
  LJContext: TJSONObject;
begin
  LFileName := GetRealFileName(ViewName);
  if not FileExists(LFileName) then
    raise EMVCFrameworkView.CreateFmt('View [%s] not found', [ViewName]);
  LTemplate := StringToUTF8(TFile.ReadAllText(LFileName, TEncoding.UTF8));
  LMEngine := TSynMustache.Parse(LTemplate);
  LJContext := TJSONObject.Create;
  try
    if FViewModel <> nil then
    begin
      for LPair in FViewModel do
      begin
        LJContext.AddPair(LPair.Key, LPair.Value)
      end;
    end;
  except
    LJContext.Free;
    raise;
  end;

  FOutput := UTF8ToString(LMEngine.RenderJSON(LJContext.ToJSON));
end;

end.
