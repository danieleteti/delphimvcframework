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
  MVCFramework.Commons, System.IOUtils, System.RTTI,
  System.Classes, Data.DB, SynMustache, SynCommons;

type
  { This class implements the mustache view engine for server side views }
  TMVCMustacheViewEngine = class(TMVCBaseViewEngine)
  strict private
    procedure PrepareModels;
  private
    class var fPartials: TSynMustachePartials;
    class var fHelpers: TSynMustacheHelpers;
    var FJSONModelAsString: string;
    procedure LoadPartials;
    procedure LoadHelpers;
  protected
    function RenderJSON(lViewEngine: TSynMustache; const JSON: RawUTF8; Partials: TSynMustachePartials;
      Helpers: TSynMustacheHelpers; OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUTF8; virtual;
  public
    procedure Execute(const ViewName: string; const OutputStream: TStream); override;
    constructor Create(const AEngine: TMVCEngine; const AWebContext: TWebContext;
      const AViewModel: TMVCViewDataObject;
      const AViewDataSets: TObjectDictionary<string, TDataSet>;
      const AContentType: string); override;
    class destructor Destroy;
  end;

  TLoadCustomHelpersProc = reference to procedure(var MustacheHelpers: TSynMustacheHelpers);

  TMVCMustacheHelpers = class sealed
  private
    class var fOnLoadCustomHelpers: TLoadCustomHelpersProc;
  protected
    class procedure RegisterHandlers(var MustacheHelpers: TSynMustacheHelpers);
    class procedure ToLowerCase(const Value: variant; out Result: variant);
    class procedure ToUpperCase(const Value: variant; out Result: variant);
    class procedure Capitalize(const Value: variant; out Result: variant);
    class procedure SnakeCase(const Value: variant; out Result: variant);
  public
    class property OnLoadCustomHelpers: TLoadCustomHelpersProc read fOnLoadCustomHelpers write fOnLoadCustomHelpers;
  end;

implementation

uses
  JsonDataObjects,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes,
  MVCFramework.Serializer.JsonDataObjects;

{$WARNINGS OFF}


type
  TSynMustacheAccess = class(TSynMustache)
  end;




var
  gPartialsLoaded : Boolean = False;
  gHelpersLoaded : Boolean = False;

constructor TMVCMustacheViewEngine.Create(const AEngine: TMVCEngine;
  const AWebContext: TWebContext; const AViewModel: TMVCViewDataObject;
  const AViewDataSets: TObjectDictionary<string, TDataSet>;
  const AContentType: string);
begin
  inherited;
  LoadPartials;
  LoadHelpers;
end;

class destructor TMVCMustacheViewEngine.Destroy;
begin
  fPartials.Free;
end;

function TMVCMustacheViewEngine.RenderJSON(lViewEngine: TSynMustache; const JSON: RawUTF8; Partials: TSynMustachePartials;
  Helpers: TSynMustacheHelpers; OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUTF8;
begin
  Result := lViewEngine.RenderJSON(JSON, Partials, Helpers, OnTranslate, EscapeInvert);
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
    lSW.Write(UTF8Tostring(RenderJSON(lViewEngine, FJSONModelAsString, fPartials, fHelpers, nil, false)));
  finally
    lSW.Free;
  end;
end;

procedure TMVCMustacheViewEngine.LoadHelpers;
begin
  if gHelpersLoaded then
  begin
    Exit
  end
  else
  begin
    TMonitor.Enter(gLock);
    try
      if not gHelpersLoaded then
      begin
        fHelpers := TSynMustache.HelpersGetStandardList;
        TMVCMustacheHelpers.RegisterHandlers(fHelpers); {dmvcframework specific helpers}
        gHelpersLoaded := True;
      end;
    finally
      TMonitor.Exit(gLock);
    end;
  end;
end;

procedure TMVCMustacheViewEngine.LoadPartials;
var
  lViewsExtension: string;
  lViewPath: string;
  lPartialName: String;
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
        lPartialFileNames := TDirectory.GetFiles(lViewPath, '*.' + lViewsExtension, TSearchOption.soAllDirectories);
        FreeAndNil(fPartials);
        fPartials := TSynMustachePartials.Create;
        for I := 0 to High(lPartialFileNames) do
        begin
          lPartialName := lPartialFileNames[i]
            .Remove(lPartialFileNames[i].Length - lViewsExtension.Length - 1)
            .Replace(TPath.DirectorySeparatorChar, '/');
          lPartialName := lPartialName.Remove(0, lViewPath.Length + 1);
          fPartials.Add(lPartialName, TFile.ReadAllText(lPartialFileNames[i]));
        end;
        gPartialsLoaded := SameText(Config[TMVCConfigKey.ViewCache], 'true');
      end;
    finally
      TMonitor.Exit(gLock);
    end;
  end;
end;

{$WARNINGS ON}


procedure TMVCMustacheViewEngine.PrepareModels;
var
  DataObj: TPair<string, TValue>;
  lDSPair: TPair<string, TDataSet>;
  lSer: TMVCJsonDataObjectsSerializer;
  lJSONModel: TJsonObject;
begin
  if Assigned(FJSONModel) and (not Assigned(ViewModel)) and (not Assigned(ViewDataSets)) then
  begin
    // if only jsonmodel is <> nil then we take the "fast path"
    FJSONModelAsString := FJSONModel.ToJSON(False);
    Exit;
  end;

  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    RegisterOptionalCustomTypesSerializers(lSer);
    if Assigned(FJSONModel) then
    begin
      lJSONModel := FJSONModel.Clone as TJsonObject;
    end
    else
    begin
      lJSONModel := TJsonObject.Create;
    end;
    try
      if Assigned(ViewModel) then
      begin
        for DataObj in ViewModel do
        begin
          lSer.TValueToJSONObjectProperty(lJSONModel, DataObj.Key, DataObj.Value, TMVCSerializationType.stDefault, nil, nil);
//          lList := TDuckTypedList.Wrap(DataObj.Value);
//          if lList <> nil then
//          begin
//            lSer.ListToJsonArray(lList, lJSONModel.A[DataObj.Key], TMVCSerializationType.stProperties, nil);
//          end
//          else
//          begin
//            lSer.ObjectToJsonObject(DataObj.Value, lJSONModel.O[DataObj.Key], TMVCSerializationType.stProperties, nil);
//          end;
        end;
      end;

      if Assigned(ViewDataSets) then
      begin
        for lDSPair in ViewDataSets do
        begin
          lSer.DataSetToJsonArray(lDSPair.Value, lJSONModel.A[lDSPair.Key], TMVCNameCase.ncAsIs, nil);
        end;
      end;
      FJSONModelAsString := lJSONModel.ToJSON(False);
    finally
      lJSONModel.Free;
    end;
  finally
    lSer.Free;
  end;
end;

{ dmvcframework specific helpers}

class procedure TMVCMustacheHelpers.Capitalize(const Value: variant;
  out Result: variant);
begin
  Result := MVCFramework.Commons.CamelCase(Value, True);
end;

class procedure TMVCMustacheHelpers.RegisterHandlers(var MustacheHelpers: TSynMustacheHelpers);
begin
  TSynMustache.HelperAdd(MustacheHelpers, 'UpperCase', TMVCMustacheHelpers.ToUpperCase);
  TSynMustache.HelperAdd(MustacheHelpers, 'LowerCase', TMVCMustacheHelpers.ToLowerCase);
  TSynMustache.HelperAdd(MustacheHelpers, 'Capitalize', TMVCMustacheHelpers.Capitalize);
  TSynMustache.HelperAdd(MustacheHelpers, 'SnakeCase', TMVCMustacheHelpers.SnakeCase);
  if Assigned(fOnLoadCustomHelpers) then
  begin
    fOnLoadCustomHelpers(MustacheHelpers);
  end;
end;

class procedure TMVCMustacheHelpers.SnakeCase(const Value: variant;
  out Result: variant);
begin
  Result := MVCFramework.Commons.SnakeCase(Value);
end;

class procedure TMVCMustacheHelpers.ToLowerCase(const Value: variant;
  out Result: variant);
begin
  Result := System.SysUtils.LowerCase(Value);
end;

class procedure TMVCMustacheHelpers.ToUpperCase(const Value: variant; out Result: variant);
begin
  Result := System.SysUtils.UpperCase(Value);
end;


end.
