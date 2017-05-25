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

unit MVCFramework.View.Renderers.Mustache;

interface

uses
  MVCFramework, System.Generics.Collections, System.SysUtils,
  MVCFramework.Commons, System.IOUtils;

type
  { This class implements the mustache view engine for server side views }
  TMVCMustacheViewEngine = class(TMVCBaseViewEngine)
  strict private
    procedure PrepareModels;
  private
    FJSONModel: string;
  public
    procedure Execute(const ViewName: string); override;
  end;

implementation

uses
  SynMustache,
  SynCommons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping;

{$WARNINGS OFF}

procedure TMVCMustacheViewEngine.Execute(const ViewName: string);
var
  ViewFileName: string;
  ViewTemplate: RawUTF8;
  ViewEngine: TSynMustache;
begin
  PrepareModels;
  ViewFileName := GetRealFileName(ViewName);
  if not FileExists(ViewFileName) then
    raise EMVCFrameworkViewException.CreateFmt('View [%s] not found', [ViewName]);
  ViewTemplate := StringToUTF8(TFile.ReadAllText(ViewFileName, TEncoding.UTF8));
  ViewEngine := TSynMustache.Parse(ViewTemplate);
  SetOutput(UTF8Tostring(ViewEngine.RenderJSON(FJSONModel)));
end;

{$WARNINGS ON}

procedure TMVCMustacheViewEngine.PrepareModels;
var
  lFirst: Boolean;
  lList: IMVCList;
  DataObj: TPair<string, TObject>;
  lSJSON: string;
  lJSON: string;
  lSer: IMVCSerializer;
begin
  if (FJSONModel <> '{}') and (not FJSONModel.IsEmpty) then
    Exit;
  FJSONModel := '{}';
  if Assigned(ViewModel) then
  begin
    lSer := GetDefaultSerializer;
    lSJSON := '{';
    lFirst := True;
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
    lSJSON := lSJSON + '}';
    FJSONModel := lSJSON;
  end;
end;

end.
