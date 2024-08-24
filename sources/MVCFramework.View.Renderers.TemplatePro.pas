// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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
    procedure Execute(const ViewName: string; const Builder: TStringBuilder); override;
  end;

implementation

uses
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping,
  TemplatePro,
  MVCFramework.Cache,
  Data.DB,
  System.Rtti,
  JsonDataObjects;

{$WARNINGS OFF}

function GetDataSetOrObjectListCount(const aValue: TValue; const aParameters: TArray<string>): TValue;
begin
  // todo
end;

function DumpAsJSONString(const aValue: TValue; const aParameters: TArray<string>): TValue;
var
  lWrappedList: IMVCList;
begin
  if not aValue.IsObject then
  begin
    Result := '(Error: Cannot serialize non-object as JSON)';
  end;

  if TDuckTypedList.CanBeWrappedAsList(aValue.AsObject, lWrappedList) then
  begin
    Result := GetDefaultSerializer.SerializeCollection(lWrappedList)
  end
  else
  begin
    if aValue.AsObject is TDataSet then
      Result := GetDefaultSerializer.SerializeDataSet(TDataSet(aValue.AsObject))
    else
      Result := GetDefaultSerializer.SerializeObject(aValue.AsObject);
  end;
end;

procedure TMVCTemplateProViewEngine.Execute(const ViewName: string; const Builder: TStringBuilder);
var
  lTP: TTProCompiler;
  lViewFileName: string;
  lViewTemplate: UTF8String;
  lCompiledTemplate: ITProCompiledTemplate;
  lPair: TPair<String, TValue>;
  lActualFileTimeStamp: TDateTime;
  lCompiledViewFileName: string;
  lActualCompiledFileTimeStamp: TDateTime;
  lUseCompiledVersion: Boolean;
  lCacheDir: string;
begin
  lUseCompiledVersion := False;
  lViewFileName := GetRealFileName(ViewName);

  if MVCUseTemplatesCache then
  begin
    lCacheDir := TPath.Combine(TPath.GetDirectoryName(lViewFileName), '__cache__');
    TDirectory.CreateDirectory(lCacheDir);
    lCompiledViewFileName := TPath.Combine(lCacheDir, TPath.ChangeExtension(TPath.GetFileName(lViewFileName), '.' + TEMPLATEPRO_VERSION + '.tpcu'));

    if not FileAge(lViewFileName, lActualFileTimeStamp) then
    begin
      raise EMVCFrameworkViewException.CreateFmt('View [%s] not found',
        [ViewName]);
    end;

    if FileAge(lCompiledViewFileName, lActualCompiledFileTimeStamp) then
    begin
      lUseCompiledVersion := lActualFileTimeStamp < lActualCompiledFileTimeStamp;
    end;
  end;

  if lUseCompiledVersion then
  begin
    lCompiledTemplate := TTProCompiledTemplate.CreateFromFile(lCompiledViewFileName);
  end
  else
  begin
    lTP := TTProCompiler.Create;
    try
      lViewTemplate := TFile.ReadAllText(lViewFileName);
      lCompiledTemplate := lTP.Compile(lViewTemplate, lViewFileName);
      if MVCUseTemplatesCache then
      begin
        lCompiledTemplate.SaveToFile(lCompiledViewFileName);
      end;
    finally
      lTP.Free;
    end;
  end;

  try
    if Assigned(ViewModel) then
    begin
      for lPair in ViewModel do
      begin
        lCompiledTemplate.SetData(lPair.Key, ViewModel[lPair.Key]);
      end;
    end;
    lCompiledTemplate.AddFilter('json', DumpAsJSONString);
    lCompiledTemplate.AddFilter('count', GetDataSetOrObjectListCount);
    Builder.Append(lCompiledTemplate.Render);
  except
    on E: ETProException do
    begin
      raise EMVCViewError.CreateFmt('View [%s] error: %s (%s)',
        [ViewName, E.Message, E.ClassName]);
    end;
  end;
end;

end.
