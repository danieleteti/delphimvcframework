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
// ***************************************************************************

unit MVCFramework.View.Renderers.TemplatePro;

interface

uses
  MVCFramework, System.Generics.Collections, System.SysUtils,
  MVCFramework.Commons, System.IOUtils, System.Classes, MVCFramework.Utils;

type
  { This class implements the TemplatePro view engine for server side views }
  TMVCTemplateProViewEngine = class(TMVCBaseViewEngine)
  public
    procedure Execute(const ViewName: string; const Builder: TStringBuilder); override;
  end;

implementation

uses
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping,
  Data.DB,
  System.Rtti,
  System.TypInfo,
  JsonDataObjects,
  TemplatePro,
  TemplatePro.Types;

type
  TCachedView = record
    Compiled: TBytes;
    ViewTimeStamp: TDateTime;
  end;

var
  // ponytail: one global lock around a dictionary lookup; fine unless profiling says otherwise
  gCompiledViews: TDictionary<string, TCachedView>;

function TryGetCachedView(const aKey: string; out aView: TCachedView): Boolean;
begin
  TMonitor.Enter(gCompiledViews);
  try
    Result := gCompiledViews.TryGetValue(aKey, aView);
  finally
    TMonitor.Exit(gCompiledViews);
  end;
end;

procedure CacheView(const aKey: string; const aView: TCachedView);
begin
  TMonitor.Enter(gCompiledViews);
  try
    gCompiledViews.AddOrSetValue(aKey, aView);
  finally
    TMonitor.Exit(gCompiledViews);
  end;
end;

{$WARNINGS OFF}

function GetDataSetOrObjectListCount(const aValue: TValue; const aParameters: TArray<TFilterParameter>): TValue;
var
  lWrappedList: IMVCList;
begin
  if not aValue.IsObject then
  begin
    Exit(False);
  end;

  if Length(aParameters) <> 0 then
  begin
    raise EMVCSSVException.Create('Expected 0 params, got ' + Length(aParameters).ToString);
  end;

  if aValue.AsObject is TDataSet then
  begin
    Result := TDataSet(aValue.AsObject).RecordCount;
  end
  else if aValue.AsObject is TJsonArray then
  begin
    Result := TJsonArray(aValue.AsObject).Count;
  end
  else if aValue.AsObject is TJsonObject then
  begin
    Result := TJsonObject(aValue.AsObject).Count;
  end
  else
  begin
    if (aValue.AsObject <> nil) and TDuckTypedList.CanBeWrappedAsList(aValue.AsObject, lWrappedList) then
    begin
      Result := lWrappedList.Count;
    end
    else
    begin
      Result := False;
    end;
  end;
end;

procedure TMVCTemplateProViewEngine.Execute(const ViewName: string; const Builder: TStringBuilder);
var
  lTP: TTProCompiler;
  lViewFileName: string;
  lViewTemplate: String;
  lCompiledTemplate: ITProCompiledTemplate;
  lPair: TPair<String, TValue>;
  lActualFileTimeStamp: TDateTime;
  lCompiledViewFileName: string;
  lActualCompiledFileTimeStamp: TDateTime;
  lUseCompiledVersion: Boolean;
  lCacheDir: string;
  lActualCalculatedFileName: String;
  lCacheKey: string;
  lCheckChanges: Boolean;
  lCachedView: TCachedView;
  lInMemory: Boolean;
begin
  lUseCompiledVersion := False;
  lInMemory := False;
  lCheckChanges := False;
  if FUseViewCache then
  begin
    // the key is what GetRealFileName resolves from: no file system access to find the cached view
    lCacheKey := FViewPath + '|' + FDefaultViewFileExtension + '|' + ViewName;
    lCheckChanges := SameText(Config[TMVCConfigKey.ViewCacheCheckChanges], 'true');
    lInMemory := TryGetCachedView(lCacheKey, lCachedView);
  end;

  if lInMemory and not lCheckChanges then
  begin
    // production: compiled once per process, never checked again (changes are picked up on restart)
    lCompiledTemplate := TTProCompiledTemplate.CreateFromBytes(lCachedView.Compiled);
  end
  else
  begin
    lViewFileName := GetRealFileName(ViewName, lActualCalculatedFileName);
    if lViewFileName.IsEmpty then
      raise EMVCSSVException.CreateFmt('View [%s] not found', [TPath.GetFileName(lActualCalculatedFileName)]);
    if FUseViewCache then
    begin
      lCacheDir := TPath.Combine(TPath.GetDirectoryName(lViewFileName), '__cache__');
      if not TDirectory.Exists(lCacheDir) then
      begin
        TDirectory.CreateDirectory(lCacheDir);
      end;
      lCompiledViewFileName := TPath.Combine(lCacheDir, TPath.ChangeExtension(TPath.GetFileName(lViewFileName), '.' + TEMPLATEPRO_VERSION + '.tpcu'));

      if not FileAge(lViewFileName, lActualFileTimeStamp) then
      begin
        raise EMVCSSVException.CreateFmt('View [%s] not found',
          [ViewName]);
      end;

      if lInMemory and (lCachedView.ViewTimeStamp = lActualFileTimeStamp) then
      begin
        lCompiledTemplate := TTProCompiledTemplate.CreateFromBytes(lCachedView.Compiled);
        lInMemory := not lCompiledTemplate.IsStale;
      end
      else
      begin
        lInMemory := False;
      end;

      if (not lInMemory) and FileAge(lCompiledViewFileName, lActualCompiledFileTimeStamp) then
      begin
        lUseCompiledVersion := lActualFileTimeStamp < lActualCompiledFileTimeStamp;
      end;
    end;

    if lUseCompiledVersion then
    begin
      lCompiledTemplate := TTProCompiledTemplate.CreateFromFile(lCompiledViewFileName);
      // a changed partial, layout or import (the view file itself is checked above by timestamp)
      lUseCompiledVersion := not lCompiledTemplate.IsStale;
    end;

    if (not lUseCompiledVersion) and (not lInMemory) then
    begin
      lTP := TTProCompiler.Create;
      try
        lViewTemplate := TFile.ReadAllText(lViewFileName);
        lCompiledTemplate := lTP.Compile(lViewTemplate, lViewFileName);
        if FUseViewCache then
        begin
          lCompiledTemplate.SaveToFile(lCompiledViewFileName);
        end;
      finally
        lTP.Free;
      end;
    end;

    if FUseViewCache and not lInMemory then
    begin
      lCachedView.Compiled := lCompiledTemplate.SaveToBytes;
      lCachedView.ViewTimeStamp := lActualFileTimeStamp;
      CacheView(lCacheKey, lCachedView);
    end;
  end;

  try
    if Assigned(ViewModel) then
    begin
      for lPair in ViewModel do
      begin
        lCompiledTemplate.SetData(lPair.Key, lPair.Value);
      end;
      if WebContext.LoggedUserExists then
      begin
        lCompiledTemplate.SetData('LoggedUserName', WebContext.LoggedUser.UserName);
      end;
    end;
    // 'json' and 'urlencode' are TemplatePro built-ins: registering custom filters with
    // those names would now replace them (custom filters win) and change the output.
    lCompiledTemplate.AddFilter('count', GetDataSetOrObjectListCount);
    lCompiledTemplate.AddFilter('fromquery',
      function (const aValue: TValue; const aParameters: TArray<TFilterParameter>): TValue
      begin
        if not aValue.IsEmpty then
        begin
          raise ETProRenderException.Create('Filter "fromquery" cannot be applied to a value [HINT] Use {{:|fromquery,"parname"}}');
        end;
        if Length(aParameters) = 1 then
        begin
          Result := Self.WebContext.Request.QueryStringParam(aParameters[0].ParStrText);
        end
        else
        begin
          raise ETProRenderException.Create('Expected 1 param for filter "fromquery", got ' + Length(aParameters).ToString);
        end;
      end);
    if Assigned(FBeforeRenderCallback) then
    begin
      FBeforeRenderCallback(TObject(lCompiledTemplate));
    end;	  
    Builder.Append(lCompiledTemplate.Render);
  except
    on E: ETProException do
    begin
      raise EMVCViewError.CreateFmt('View [%s] error: %s (%s)',
        [ViewName, E.Message, E.ClassName]);
    end;
  end;
end;

initialization

gCompiledViews := TDictionary<string, TCachedView>.Create;

finalization

gCompiledViews.Free;

end.
