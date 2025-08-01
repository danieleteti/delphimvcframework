// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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
  System.Classes, Data.DB, MVCFramework.IntfObjectPool,
  mormot.core.mustache, mormot.core.unicode;

type
  { This class implements the mustache view engine for server side views }
  TMVCMustacheViewEngine = class(TMVCBaseViewEngine)
  strict private
    procedure PrepareModels;
  private
    fModelPrepared: Boolean;
    class var fPartials: TSynMustachePartials;
    class var fHelpers: TSynMustacheHelpers;
    var FJSONModelAsString: string;
    procedure LoadPartials;
    procedure LoadHelpers;
  protected
    function RenderJSON(lViewEngine: TSynMustache; const JSON: UTF8String; Partials: TSynMustachePartials;
      Helpers: TSynMustacheHelpers; OnTranslate: TOnStringTranslate; EscapeInvert: boolean): UTF8String; virtual;
  public
    procedure Execute(const ViewName: string; const Builder: TStringBuilder); override;
    constructor Create(const AEngine: TMVCEngine; const AWebContext: TWebContext;
      const AController: TMVCController;
      const AViewModel: TMVCViewDataObject;
      const AContentType: string); override;
    class destructor Destroy;
    class constructor Create;
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
  Types,
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
  const AWebContext: TWebContext; const AController: TMVCController;
  const AViewModel: TMVCViewDataObject; const AContentType: string);
begin
  inherited;
  fModelPrepared := False;
  LoadPartials;
  LoadHelpers;
end;

class constructor TMVCMustacheViewEngine.Create;
begin
end;

class destructor TMVCMustacheViewEngine.Destroy;
begin

end;

function TMVCMustacheViewEngine.RenderJSON(lViewEngine: TSynMustache; const JSON: UTF8String; Partials: TSynMustachePartials;
  Helpers: TSynMustacheHelpers; OnTranslate: TOnStringTranslate; EscapeInvert: boolean): UTF8String;
begin
  Result := lViewEngine.RenderJSON(JSON, Partials, Helpers, OnTranslate, EscapeInvert);
end;

procedure TMVCMustacheViewEngine.Execute(const ViewName: string; const Builder: TStringBuilder);
var
  lViewFileName: string;
  lViewTemplate: UTF8String;
  lViewEngine: TSynMustache;
  lActualCalculatedFileName: String;
begin
  PrepareModels;
  lViewFileName := GetRealFileName(ViewName, lActualCalculatedFileName);
  if lViewFileName.IsEmpty then
    raise EMVCSSVException.CreateFmt('View [%s] not found', [TPath.GetFileName(lActualCalculatedFileName)]);
  lViewTemplate := StringToUTF8(TFile.ReadAllText(lViewFileName, TEncoding.UTF8));
  lViewEngine := TSynMustache.Parse(lViewTemplate);
  Builder.Append(UTF8Tostring(RenderJSON(lViewEngine, FJSONModelAsString, fPartials, fHelpers, nil, false)));
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
  lPartialFileNames: TStringDynArray;
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
        lViewPath := Config.Value[TMVCConfigKey.ViewPath];
        if TPath.IsRelativePath(lViewPath) then
          lViewPath := TMVCBase.GetApplicationFileNamePath + lViewPath;
        lPartialFileNames := TDirectory.GetFiles(lViewPath, '*.' + lViewsExtension, TSearchOption.soAllDirectories);
        FreeAndNil(fPartials);
        fPartials := TSynMustachePartials.Create;
        for I := 0 to High(lPartialFileNames) do
        begin
          lPartialName := lPartialFileNames[i]
            .Remove(lPartialFileNames[i].Length - lViewsExtension.Length - 1)
            .Replace(TPath.DirectorySeparatorChar, '/');
          lPartialName := lPartialName.Remove(0, lViewPath.Length + 1);
          fPartials.Add(lPartialName, TFile.ReadAllText(lPartialFileNames[i], TEncoding.UTF8));
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
  lSer: TMVCJsonDataObjectsSerializer;
  lJSONModel: TJsonObject;
begin
  if fModelPrepared then
  begin
    Exit;
  end;

  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    RegisterOptionalCustomTypesSerializers(lSer);
    lJSONModel := TJsonObject.Create;
    try
      if Assigned(ViewModel) then
      begin
        for DataObj in ViewModel do
        begin
          TMVCJsonDataObjectsSerializer(lSer).TValueToJSONObjectProperty(lJSONModel, DataObj.Key, DataObj.Value, TMVCSerializationType.stDefault, nil, nil);
        end;
      end;
      FJSONModelAsString := lJSONModel.ToJSON(False);
    finally
      lJSONModel.Free;
    end;
  finally
    lSer.Free;
  end;
  fModelPrepared := True;
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



initialization

finalization

FreeAndNil(TMVCMustacheViewEngine.fPartials);

end.
