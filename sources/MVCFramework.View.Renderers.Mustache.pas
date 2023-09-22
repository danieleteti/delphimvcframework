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

unit MVCFramework.View.Renderers.Mustache;

{$IFDEF LINUX}
This unit is not compatible with Linux
{$ENDIF}
interface

uses
  MVCFramework, System.SysUtils, System.Generics.Collections,
  MVCFramework.Commons, System.IOUtils, System.Classes, Data.DB, SynMustache;

type
  { This class implements the mustache view engine for server side views }
  TMVCMustacheViewEngine = class(TMVCBaseViewEngine)
  strict private
    procedure PrepareModels;
  private
    class var fPartials: TSynMustachePartials;
    var FJSONModelAsString: string;
    procedure LoadPartials;
  public
    procedure Execute(const ViewName: string; const OutputStream: TStream); override;
    constructor Create(const AEngine: TMVCEngine; const AWebContext: TWebContext;
      const AViewModel: TMVCViewDataObject;
      const AViewDataSets: TObjectDictionary<string, TDataSet>;
      const AContentType: string); override;
    class destructor Destroy;
  end;

implementation

uses
  SynCommons,
  JsonDataObjects,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes,
  MVCFramework.Serializer.JsonDataObjects;

{$WARNINGS OFF}


type
  TSynMustacheAccess = class(TSynMustache)
  end;

var
  gPartialsLoaded : Boolean = False;

constructor TMVCMustacheViewEngine.Create(const AEngine: TMVCEngine;
  const AWebContext: TWebContext; const AViewModel: TMVCViewDataObject;
  const AViewDataSets: TObjectDictionary<string, TDataSet>;
  const AContentType: string);
begin
  inherited;
  LoadPartials;
end;

class destructor TMVCMustacheViewEngine.Destroy;
begin
  fPartials.Free;
end;

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
  lViewEngine := TSynMustache.Parse(lViewTemplate);
  lSW := TStreamWriter.Create(OutputStream);
  try
    lSW.Write(UTF8Tostring(lViewEngine.RenderJSON(FJSONModelAsString, fPartials, nil, nil)));
  finally
    lSW.Free;
  end;
end;

procedure TMVCMustacheViewEngine.LoadPartials;
var
  lViewsExtension: string;
  lViewPath: string;
  lPartialFileNames: TArray<string>;
  I: Integer;
begin
  if gPartialsLoaded then
  begin
    Exit
  end
  else
  begin
    TMonitor.Enter(gLock);
    try
      if not gPartialsLoaded then
      begin
        lViewsExtension := Config[TMVCConfigKey.DefaultViewFileExtension];
        lViewPath := Config[TMVCConfigKey.ViewPath];
        lPartialFileNames := TDirectory.GetFiles(lViewPath, '*.' + lViewsExtension);
        fPartials := TSynMustachePartials.Create;
        for I := 0 to High(lPartialFileNames) do
        begin
          fPartials.Add(TPath.GetFileNameWithoutExtension(lPartialFileNames[i]), TFile.ReadAllText(lPartialFileNames[i]));
        end;
        gPartialsLoaded := True;
      end;
    finally
      TMonitor.Exit(gLock);
    end;
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
  if Assigned(FJSONModel) then
  begin
    FJSONModelAsString := FJSONModel.ToJSON(False);
    Exit;
  end;

  {TODO -oDanieleT -cGeneral : Quite inefficient to generate JSON in this way. Why don't use a JSONObject directly?}
  if (FJSONModelAsString <> '{}') and (not FJSONModelAsString.IsEmpty) then
    Exit;
  FJSONModelAsString := '{}';

  lSer := TMVCJsonDataObjectsSerializer.Create;
  RegisterOptionalCustomTypesSerializers(lSer);
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
  FJSONModelAsString := lSJSON;
end;

end.
