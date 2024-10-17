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
var
  lWrappedList: IMVCList;
begin
  if not aValue.IsObject then
  begin
    Result := False;
  end;

  if Length(aParameters) <> 0 then
  begin
    Result := '(Error: Expected 0 params, got ' + Length(aParameters).ToString + ')';
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
  lViewTemplate: String;
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
  if lViewFileName.IsEmpty then
    raise EMVCSSVException.CreateFmt('View [%s] not found', [ViewName]);
  if FUseViewCache then
  begin
    lCacheDir := TPath.Combine(TPath.GetDirectoryName(lViewFileName), '__cache__');
    TDirectory.CreateDirectory(lCacheDir);
    lCompiledViewFileName := TPath.Combine(lCacheDir, TPath.ChangeExtension(TPath.GetFileName(lViewFileName), '.' + TEMPLATEPRO_VERSION + '.tpcu'));

    if not FileAge(lViewFileName, lActualFileTimeStamp) then
    begin
      raise EMVCSSVException.CreateFmt('View [%s] not found',
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
      if FUseViewCache then
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
    lCompiledTemplate.AddFilter('fromquery',
      function (const aValue: TValue; const aParameters: TArray<string>): TValue
      begin
        if not aValue.IsEmpty then
        begin
          raise ETProRenderException.Create('Filter "fromquery" cannot be applied to a value [HINT] Use {{:|fromquery,"parname"}}');
        end;
        if Length(aParameters) = 1 then
        begin
          Result := Self.WebContext.Request.QueryStringParam(aParameters[0]);
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

end.
