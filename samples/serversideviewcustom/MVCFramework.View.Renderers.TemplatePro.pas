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
// ***************************************************************************

unit MVCFramework.View.Renderers.TemplatePro;

interface

uses
  MVCFramework, System.Generics.Collections, System.SysUtils,
  MVCFramework.Commons, System.IOUtils, System.Classes;

type
  { This class implements the TemplatePro view engine for server side views }
  TMVCTemplateProViewEngine = class(TMVCBaseViewEngine)
  public
    procedure Execute(const ViewName: string;
      const OutputStream: TStream); override;
  end;

implementation

uses
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping,
  TemplateProU,
  MVCFramework.Cache,
  Data.DB, System.Rtti;

{$WARNINGS OFF}

procedure TMVCTemplateProViewEngine.Execute(const ViewName: string;
  const OutputStream: TStream);
var
  lTP: TTemplateProEngine;
  lViewFileName: string;
  lViewTemplate: UTF8String;
  lCacheItem: TMVCCacheItem;
begin
  lTP := TTemplateProEngine.Create;
  try
    lViewFileName := GetRealFileName(ViewName);
    if not FileExists(lViewFileName) then
    begin
      raise EMVCFrameworkViewException.CreateFmt('View [%s] not found',
        [ViewName]);
    end;

    if not TMVCCacheSingleton.Instance.ContainsItem(lViewFileName, lCacheItem)
    then
    begin
      lViewTemplate := TFile.ReadAllText(lViewFileName, TEncoding.UTF8);
      lCacheItem := TMVCCacheSingleton.Instance.SetValue(lViewFileName,
        lViewTemplate);
    end
    else
    begin
      if lCacheItem.TimeStamp < TFile.GetLastWriteTime(lViewFileName) then
      begin
        lViewTemplate := TFile.ReadAllText(lViewFileName, TEncoding.UTF8);
        TMVCCacheSingleton.Instance.SetValue(lViewFileName,
          lViewTemplate);
      end;
    end;

    lViewTemplate := lCacheItem.Value.AsString;
    try
      lTP.Execute(lViewTemplate, TTPObjectListDictionary(ViewModel),
        TTPDatasetDictionary(ViewDataSets), OutputStream);
    except
      on E: EParserException do
      begin
        raise EMVCViewError.CreateFmt('View [%s] error: %s (%s)',
          [ViewName, E.Message, E.ClassName]);
      end;
    end;
  finally
    lTP.Free;
  end;
end;

end.
