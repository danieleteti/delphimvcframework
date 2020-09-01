// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.View.Renderers.Mustache;

{$IFDEF LINUX}
This unit is not compatible with Linux
{$ENDIF}

interface

uses
  MVCFramework, System.SysUtils,
  MVCFramework.Commons, System.IOUtils, System.Classes, Data.DB;

type
  { This class implements the mustache view engine for server side views }
  TMVCMustacheViewEngine = class(TMVCBaseViewEngine)
  strict private
    procedure PrepareModels;
  private
    FJSONModel: string;
  public
    procedure Execute(const ViewName: string; const OutputStream: TStream);
      override;
  end;

implementation

uses
  SynMustache,
  SynCommons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping, System.Generics.Collections;

{$WARNINGS OFF}


procedure TMVCMustacheViewEngine.Execute(const ViewName: string; const OutputStream: TStream);
var
  lViewFileName: string;
  lViewTemplate: RawUTF8;
  lViewEngine: TSynMustache;
  lSW: TStreamWriter;
begin
  PrepareModels;
  lViewFileName := GetRealFileName(ViewName);
  if not FileExists(lViewFileName) then
    raise EMVCFrameworkViewException.CreateFmt('View [%s] not found', [ViewName]);
  lViewTemplate := StringToUTF8(TFile.ReadAllText(lViewFileName, TEncoding.UTF8));
  { don't free this instance! There is a garbagecollector in SynCommons }
  lViewEngine := TSynMustache.Parse(lViewTemplate);
  lSW := TStreamWriter.Create(OutputStream);
  try
    lSW.Write(UTF8Tostring(lViewEngine.RenderJSON(FJSONModel)));
  finally
    lSW.Free;
  end;
end;

{$WARNINGS ON}


procedure TMVCMustacheViewEngine.PrepareModels;
var
  lFirst: Boolean;
  lList: IMVCList;
  DataObj: TPair<string, TObject>;
  lDSPair: TPair<string, TDataSet>;
  lSJSON: string;
  lJSON: string;
  lSer: IMVCSerializer;
begin
  if (FJSONModel <> '{}') and (not FJSONModel.IsEmpty) then
    Exit;
  FJSONModel := '{}';

  lSer := GetDefaultSerializer;
  lSJSON := '{';
  lFirst := True;

  if Assigned(ViewModel) then
  begin
    for DataObj in ViewModel do
    begin
      lList := TDuckTypedList.Wrap(DataObj.Value);
      if lList <> nil then
        lJSON := lSer.SerializeCollection(DataObj.Value)
      else
        lJSON := lSer.SerializeObject(DataObj.Value);
      if not lFirst then
        lSJSON := lSJSON + ',';
      lSJSON := lSJSON + '"' + DataObj.Key + '":' + lJSON;
      lFirst := False;
    end;
  end;

  if Assigned(ViewDataSets) then
  begin
    for lDSPair in ViewDataSets do
    begin
      lJSON := lSer.SerializeDataSet(lDSPair.Value);
      if not lFirst then
        lSJSON := lSJSON + ',';
      lSJSON := lSJSON + '"' + lDSPair.Key + '":' + lJSON;
      lFirst := False;
    end;
  end;
  lSJSON := lSJSON + '}';
  FJSONModel := lSJSON;
end;

end.
