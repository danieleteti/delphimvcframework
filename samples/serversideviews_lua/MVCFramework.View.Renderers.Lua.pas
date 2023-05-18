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

unit MVCFramework.View.Renderers.Lua;

{$WARNINGS OFF}
{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections,
  Data.DB,
  MVCFramework.View.Cache,
  MVCFramework.Serializer.JsonDataObjects,
  System.SysUtils, System.Classes;

type
  TMVCLuaViewEngine = class(TMVCBaseViewEngine)
  private
    function GetCompiledFileName(const FileName: string): string;
  public
    procedure Execute(const ViewName: string;
      const OutputStream: TStream); override;
  protected
    function IsCompiledVersionUpToDate(const FileName, CompiledFileName: string)
      : Boolean; override;
  public const
    LOG_FILE_NAME = 'mvc_%s.log';
    DEFAULT_VIEW_EXT = 'elua';
  end;

implementation

uses
  MVCFramework.DuckTyping,
  LuaBind,
  LuaBind.Filters.Text,
  LuaBind.CustomType.DataSet,
  LuaBind.Intf,
  System.ioutils,
  System.JSON,
  JsonDataObjects, MVCFramework.Serializer.Commons, System.Rtti;

function __lua_stream_out(L: Plua_State): Integer; cdecl;
var
  lPointerToStreamWriter: Pointer;
  lData: PAnsiChar;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number, expected 2'));
    Exit;
  end;
  lData := lua_tostring(L, -1);
  lPointerToStreamWriter := lua_topointer(L, -2);
  TStreamWriter(lPointerToStreamWriter).Write(UTF8Encode(lData));
  Result := 0;
end;

{ TMVCEmbeddedLuaView }

procedure TMVCLuaViewEngine.Execute(const ViewName: string;
  const OutputStream: TStream);
var
  Lua: TLuaEngine;
  lDataSetName: string;
  lLuaFilter: TLuaEmbeddedTextFilter;
  lViewFileName: String;
  lFileName, lCompiledFileName: string;
  lLuaCompiledCode: string;
  lStreamReader: TStreamReader;
  DecodeJSONStrings: string;
  lSer: TMVCJsonDataObjectsSerializer;
  lJSONStr: string;
  lScriptOutput: TStreamWriter;
begin
  lScriptOutput := TStreamWriter.Create(OutputStream, TEncoding.UTF8);
  try
    Lua := TLuaEngine.Create;
    try
      Lua.LoadExternalLibraries(TLuaDataSetExposerLibraries.Create);

      lViewFileName := StringReplace(ViewName, '/', '\', [rfReplaceAll]);
      // $0.02 of normalization
      if lViewFileName = '\' then
        lViewFileName := '\index.' + DEFAULT_VIEW_EXT
      else
        lViewFileName := lViewFileName + '.' + DEFAULT_VIEW_EXT;

      if DirectoryExists(Config.Value[TMVCConfigKey.ViewPath]) then
        lFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(Config.Value[TMVCConfigKey.ViewPath]) +
          lViewFileName)
      else
        lFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
          Config.Value[TMVCConfigKey.ViewPath]) + lViewFileName);
      lViewFileName := lFileName;

      if not FileExists(lViewFileName) then
        raise EMVCViewError.CreateFmt('View [%s.%s] not found',
          [ViewName, DEFAULT_VIEW_EXT])
      else
      begin
        DecodeJSONStrings := '';
        { continuare da questo problema }
        if Assigned(ViewModel) then
        begin
          for lDataSetName in ViewModel.Keys do
          begin
            lSer := TMVCJsonDataObjectsSerializer.Create(nil);
            try
              if TDuckTypedList.CanBeWrappedAsList(ViewModel[lDataSetName]) then
              begin
                lJSONStr := lSer.SerializeCollection(ViewModel[lDataSetName]);
              end
              else
              begin //PODO
                lJSONStr := lSer.SerializeObject(ViewModel[lDataSetName]);
              end;
            finally
              lSer.Free;
            end;
            Lua.DeclareGlobalString(lDataSetName, lJSONStr);
            DecodeJSONStrings := DecodeJSONStrings + sLineBreak + ' ' +
              AnsiString(lDataSetName) + ' = json.decode(' + AnsiString(lDataSetName) + ')';
          end;
        end;
      end;
      if Assigned(ViewDataSets) then
      begin
        for lDataSetName in ViewDataSets.Keys do
        begin
          ExposeDataSet(Lua, ViewDataSets[lDataSetName], ViewDataSets[lDataSetName].Name);
        end;
      end;

      Lua.DeclareGlobalString('__ROOT__', ExtractFilePath(ParamStr(0)));
      Lua.DeclareGlobalString('__log_file', LOG_FILE_NAME);
      Lua.DeclareGlobalLightUserData('__webcontext', WebContext);
      Lua.DeclareGlobalLightUserData('__stringbuilder', lScriptOutput);
      lCompiledFileName := GetCompiledFileName(lViewFileName);

      if not IsCompiledVersionUpToDate(lViewFileName, lCompiledFileName) then
      begin
        TMonitor.Enter(Self);
        try
          if not IsCompiledVersionUpToDate(lViewFileName, lCompiledFileName) then
          begin
            lLuaFilter := TLuaEmbeddedTextFilter.Create;
            try
              lLuaFilter.OutputFunction := 'elua_out';
              lLuaFilter.TemplateCode := TFile.ReadAllText(lViewFileName,
                TEncoding.UTF8);
              lLuaFilter.Execute;
              TFile.WriteAllText(lCompiledFileName,
                lLuaFilter.LuaCode,
                TEncoding.ANSI);
            finally
              lLuaFilter.Free;
            end;
          end;
        finally
          TMonitor.Exit(Self);
        end;
      end;

      // read lua compiled code
      lStreamReader := TStreamReader.Create(TFileStream.Create(lCompiledFileName,
        fmOpenRead or fmShareDenyNone), TEncoding.UTF8);
      try
        lStreamReader.OwnStream;
        lLuaCompiledCode := lStreamReader.ReadToEnd;
      finally
        lStreamReader.Free;
      end;
      Lua.DeclareGlobalFunction('internal_elua_out', @__lua_stream_out);
      Lua.LoadScript('require "Lua.boot" ' + sLineBreak + DecodeJSONStrings + sLineBreak + lLuaCompiledCode);
      // EXECUTE IT!!!
      Lua.Execute;
    finally
      Lua.Free;
    end;
  finally
    FreeAndNil(lScriptOutput);
  end;
end;

function TMVCLuaViewEngine.GetCompiledFileName(const FileName: string): string;
var
  CompiledFileDir: string;
  CompiledFileName: string;
begin
  // Exit(FileName + '.compiled');
  CompiledFileDir := IncludeTrailingPathDelimiter(ExtractFilePath(FileName) +
    '__compiled');
  CompiledFileName := CompiledFileDir +
    ChangeFileExt(ExtractFileName(FileName), '.lua');
  ForceDirectories(CompiledFileDir);
  Result := CompiledFileName;
end;

function TMVCLuaViewEngine.IsCompiledVersionUpToDate(const FileName,
  CompiledFileName: string): Boolean;
var
  dt1: TDateTime;
  dt2: TDateTime;
begin
  dt2 := 0;
  FileAge(FileName, dt1);
  FileAge(CompiledFileName, dt2);
  Result := dt1 < dt2;
end;

end.
