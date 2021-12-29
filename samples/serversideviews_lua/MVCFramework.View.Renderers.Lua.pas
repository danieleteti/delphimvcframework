// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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

//function __lua_form_parameter(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//begin
//  if lua_gettop(L) <> 2 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -2, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  res := WebContext.Request.Params[parname];
//  TLuaUtils.PushTValue(L, res);
//  Result := 1;
//end;
//
//function __lua_headers_get_all(L: Plua_State): Integer; cdecl;
//var
//  // parname: string;
//  res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//begin
//  if lua_gettop(L) <> 1 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number (0 expected)'));
//    Exit;
//  end;
//  lua_getfield(L, -1, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  res := WebContext.Request.RawWebRequest.Content;
//  { TODO -oDaniele -cGeneral : Do not works }
//  TLuaUtils.PushTValue(L, res);
//  Result := 1;
//end;
//
//function __lua_headers(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//  // parvalue: string;
//begin
//  if (lua_gettop(L) <> 2) then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -2, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  res := WebContext.Request.RawWebRequest.GetFieldByName(parname.ToUpper);
//  TLuaUtils.PushTValue(L, res);
//  Result := 1;
//end;
//
//function __lua_set_http_code(L: Plua_State): Integer; cdecl;
//var
//  // parname: string;
//  // res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//  // parvalue: string;
//  errocode: Integer;
//begin
//  if lua_gettop(L) <> 2 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//
//  // setting the http return code    request:http_code(404)
//  errocode := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtInteger, L,
//    -1).AsInteger;
//  lua_getfield(L, -2, '__self');
//  p := lua_topointer(L, -1);
//
//  WebContext := TWebContext(TObject(p));
//  WebContext.Response.StatusCode := errocode;
//  Result := 0;
//end;
//
//function __lua_set_response_headers(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  // res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//  parvalue: string;
//begin
//  if lua_gettop(L) <> 3 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//
//  // setting an header  request:headers(name, newvalue)
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -2).AsString;
//  parvalue := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -3, '__self');
//  p := lua_topointer(L, -1);
//
//  WebContext := TWebContext(TObject(p));
//  WebContext.Response.CustomHeaders.Values[parname] := parvalue;
//  Result := 0;
//end;
//
//function __lua_post_parameter(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//begin
//  if lua_gettop(L) <> 2 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -2, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  res := WebContext.Request.ContentParam(parname);
//  TLuaUtils.PushTValue(L, res);
//  Result := 1;
//end;
//
//function __lua_set_header_field(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  // res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//  parvalue: string;
//begin
//  if lua_gettop(L) <> 2 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -2).AsString;
//  parvalue := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -3, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  WebContext.Response.SetCustomHeader(parname, parvalue);
//  Result := 0;
//end;
//
//function __lua_get_parameter(L: Plua_State): Integer; cdecl;
//var
//  parname: string;
//  res: string;
//  // rq: TMVCWebRequest;
//  p: Pointer;
//  WebContext: TWebContext;
//begin
//  if lua_gettop(L) <> 2 then
//  begin
//    luaL_error(L, PAnsiChar('Wrong parameters number'));
//    Exit;
//  end;
//  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
//    -1).AsString;
//  lua_getfield(L, -2, '__self');
//  p := lua_topointer(L, -1);
//  WebContext := TWebContext(TObject(p));
//  res := WebContext.Request.QueryStringParam(parname);
//  TLuaUtils.PushTValue(L, res);
//  Result := 1;
//end;

// function __lua_stream_out(L: Plua_State): Integer; cdecl;
// var
// p: Pointer;
// WebContext: TWebContext;
// v: string;
// begin
// if lua_gettop(L) <> 2 then
// begin
// luaL_error(L, PAnsiChar('Wrong parameters number'));
// Exit;
// end;
// v := lua_tostring(L, -1);
// lua_getfield(L, -2, '__self');
// p := lua_topointer(L, -1);
//
// WebContext := TWebContext(TObject(p));
// raise Exception.Create('WIP');
// // TStringBuilder(WebContext.ReservedData).Append(v);
// Result := 0;
// end;

function __lua_stream_out(L: Plua_State): Integer; cdecl;
var
  lPointerToWebContext, lPointerToStreamWriter: Pointer;
  WebContext: TWebContext;
  v: PAnsiChar;
  lParCount: Integer;
  lArr: TArray<Byte>;
begin
  lParCount := lua_gettop(L);
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  v := lua_tostring(L, -1);

  lPointerToStreamWriter := lua_topointer(L, -2);
//  lua_getfield(L, -2, '__stringbuilder');
//  lPointerToStringBuilder := lua_topointer(L, -1);

//  lua_getfield(L, -2, '__self');
//  lPointerToWebContext := lua_topointer(L, -1);
  //TWebContext(TObject(lPointerToWebContext)).Response.Content := v;

//  SetLength(lArr, Length(v));
//  CopyArray(lArr, v, TypeInfo(byte), Length(lArr));

  TStreamWriter(lPointerToStreamWriter).Write(UTF8Encode(v));
  Result := 0;
end;


// var
// s: string;
// o: TObject;
// C: TWebContext;
// begin
// if lua_gettop(L) <> 2 then
// begin
// luaL_error(L, PAnsiChar('Wrong parameters number'));
// Exit;
// end;
//
// if lua_isstring(L, - 1) = 1 then
// begin
// s := lua_tostring(L, - 1);
// // lua_pop(L, 1);
// end
// else
// begin
// luaL_error(L, PAnsiChar('Type mismatch, expected String'));
// Exit;
// end;
//
// // if lua_islightuserdata(L, - 2) then
// // begin
// o := TObject(lua_topointer(L, - 2));
// // lua_pop(L, 1);
// // end
// // else
// // begin
// // luaL_error(L, PAnsiChar('Type mismatch, expected LightUserData'));
// // Exit;
// // end;
//
// C := o as TWebContext;
// TStringBuilder(C.ReservedData).Append(s);
// Result := 0;
// end;

{ TMVCEmbeddedLuaView }

procedure TMVCLuaViewEngine.Execute(const ViewName: string;
  const OutputStream: TStream);
var
  Lua: TLuaEngine;
  k: string;
  LuaFilter: TLuaEmbeddedTextFilter;
  lViewFileName: String;
  _FFileName, CompiledFileName: string;
  v: string;
  sr: TStreamReader;
  DecodeJSONStrings: string;
  LuaRequestFunctions: TDictionary<string, lua_CFunction>;
  LuaResponseFunctions: TDictionary<string, lua_CFunction>;
  pn: string;
  lSer: TMVCJsonDataObjectsSerializer;
  lJSONStr: string;
  lJSONObj: TJDOJsonObject;
  lJSONArr: TJDOJsonArray;
  LuaResponseData: TDictionary<string, TValue>;
  ScriptOutputStringBuilder: TStreamWriter;
begin
  ScriptOutputStringBuilder := TStreamWriter.Create(OutputStream, TEncoding.UTF8);
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
        _FFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(Config.Value[TMVCConfigKey.ViewPath]) +
          lViewFileName)
      else
        _FFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
          Config.Value[TMVCConfigKey.ViewPath]) + lViewFileName);
      lViewFileName := _FFileName;

      if not FileExists(lViewFileName) then
        raise EMVCViewError.CreateFmt('View [%s.%s] not found',
          [ViewName, DEFAULT_VIEW_EXT])
      else
      begin
        DecodeJSONStrings := '';
        { continuare da questo problema }
        if Assigned(ViewModel) then
        begin
          for k in ViewModel.Keys do
          begin
            if not(ViewModel[k] is TJDOJsonBaseObject) then
            begin
              lSer := TMVCJsonDataObjectsSerializer.Create(nil);
              try
                if TDuckTypedList.CanBeWrappedAsList(ViewModel[k]) then
                begin
                  //List
                  lJSONArr := TJDOJsonArray.Create;
                  try
                    lSer.ListToJsonArray(TDuckTypedList.Wrap(ViewModel[k],
                      False), lJSONArr,
                      TMVCSerializationType.stDefault, [], nil);
                    lJSONStr := lJSONArr.ToJSON(True);
                  finally
                    lJSONArr.Free;
                  end;
                end
                else if ViewModel[k] is System.JSON.TJSONValue then
                begin
                  //System.JSON
                  lJSONStr := System.JSON.TJSONValue(ViewModel[k]).ToJSON;
                end
                else
                begin //PODO
                  lJSONObj := TJDOJsonObject.Create;
                  try
                    lSer.ObjectToJSONObject(ViewModel[k], lJSONObj,
                      TMVCSerializationType.stDefault, []);
                    lJSONStr := lJSONObj.ToJSON(True);
                  finally
                    lJSONObj.Free;
                  end;
                end;
              finally
                lSer.Free;
              end;
            end
            else
            begin
              //JsonDataObjects
              lJSONStr := TJDOJsonBaseObject(ViewModel[k]).ToJSON(True);
            end;
            Lua.DeclareGlobalString(k, lJSONStr);
            DecodeJSONStrings := DecodeJSONStrings + sLineBreak + 'local ' +
              AnsiString(k) + ' = json.decode(' + AnsiString(k) + ')';
          end;
        end;
      end;
      if Assigned(ViewDataSets) then
      begin
        for k in ViewDataSets.Keys do
        begin
          ExposeDataSet(Lua, ViewDataSets[k], ViewDataSets[k].Name);
        end;
      end;

      Lua.DeclareGlobalString('__ROOT__', ExtractFilePath(ParamStr(0)));
      Lua.DeclareGlobalString('__log_file', LOG_FILE_NAME);
      Lua.DeclareGlobalLightUserData('__webcontext', WebContext);
      Lua.DeclareGlobalLightUserData('__stringbuilder', ScriptOutputStringBuilder);
      CompiledFileName := GetCompiledFileName(lViewFileName);

      TMonitor.Enter(Self);
      try
        if true or (not IsCompiledVersionUpToDate(lViewFileName, CompiledFileName)) then
        begin
          LuaFilter := TLuaEmbeddedTextFilter.Create;
          try
            LuaFilter.OutputFunction := 'elua_out';
            LuaFilter.TemplateCode := TFile.ReadAllText(lViewFileName,
              TEncoding.ANSI);
            LuaFilter.Execute;
            TFile.WriteAllText(CompiledFileName,
              LuaFilter.LuaCode,
              TEncoding.ANSI);
          finally
            LuaFilter.Free;
          end;
        end;
      finally
        TMonitor.Exit(Self);
      end;

      // read lua compiled code
      sr := TStreamReader.Create(TFileStream.Create(CompiledFileName,
        fmOpenRead or fmShareDenyNone), TEncoding.UTF8);
      try
        sr.OwnStream;
        v := sr.ReadToEnd;
      finally
        sr.Free;
      end;
      Lua.DeclareGlobalFunction('internal_elua_out', @__lua_stream_out);
      Lua.LoadScript('require "Lua.boot" ' + sLineBreak + DecodeJSONStrings + sLineBreak + v);
      // EXECUTE IT!!!
      Lua.Execute;
    finally
      Lua.Free;
    end;
  finally
    FreeAndNil(ScriptOutputStringBuilder);
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
