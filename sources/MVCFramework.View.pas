unit MVCFramework.View;
{$WARNINGS OFF}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections,
  Data.DB,
  MVCFramework.View.Cache,
  System.SysUtils;

type
  TMVCBaseView = class(TMVCBase)
  private
    FViewName: string;
    FWebContext: TWebContext;
    FViewModel: TMVCDataObjects;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FMVCEngine: TMVCEngine;
    // FViewCache   : TViewCache;
    // procedure SetViewCache(const Value: TViewCache);

  strict protected
    FCurrentContentType: string;

  protected
    function IsCompiledVersionUpToDate(const FileName, CompiledFileName: string)
      : Boolean; virtual; abstract;
    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;

  public
    constructor Create(AViewName: string; AMVCEngine: TMVCEngine;
      AWebContext: TWebContext; AViewModels: TMVCDataObjects;
      AViewDataSets: TObjectDictionary<string, TDataSet>;
      ACurrentContentType: string); virtual;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    // property ViewCache: TViewCache read FViewCache write SetViewCache;
  end;

  TMVCEmbeddedLuaView = class(TMVCBaseView)

  private
    ScriptOutputStringBuilder: TStringBuilder;
    FFileName: string;
    function GetCompiledFileName(const FileName: string): string;

  private
    FOutput: string;

  private
    procedure SetOutput(const Value: string);
    procedure SetFileName(const Value: string);

  public
    procedure Execute; override;
    property Output: string read FOutput write SetOutput;
    property FileName: string read FFileName write SetFileName;

  protected
    function IsCompiledVersionUpToDate(const FileName, CompiledFileName: string)
      : Boolean; override;

  public const
    LOG_FILE_NAME = 'mvc_%s.log';
    DEFAULT_VIEW_EXT = 'elua';
  end;

implementation

uses
  LuaBind,
  LuaBind.Filters.Text,
  LuaBind.CustomType.DataSet,
  LuaBind.Intf,
  System.ioutils,
  System.Classes
{$IF CompilerVersion < 27}
    , Data.DBXJSON
{$ELSE}
    , System.JSON
{$ENDIF};

function __lua_form_parameter(L: Plua_State): Integer; cdecl;
var
  parname: string;
  res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  res := WebContext.Request.Params[parname];
  TLuaUtils.PushTValue(L, res);
  Result := 1;
end;

function __lua_headers_get_all(L: Plua_State): Integer; cdecl;
var
  // parname: string;
  res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
begin
  if lua_gettop(L) <> 1 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number (0 expected)'));
    Exit;
  end;
  lua_getfield(L, -1, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  res := WebContext.Request.RawWebRequest.RawContent;
  { TODO -oDaniele -cGeneral : Do not works }
  TLuaUtils.PushTValue(L, res);
  Result := 1;
end;

function __lua_headers(L: Plua_State): Integer; cdecl;
var
  parname: string;
  res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
  // parvalue: string;
begin
  if (lua_gettop(L) <> 2) then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;

  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  res := WebContext.Request.RawWebRequest.GetFieldByName(parname.ToUpper);
  TLuaUtils.PushTValue(L, res);
  Result := 1;
end;

function __lua_set_http_code(L: Plua_State): Integer; cdecl;
var
  // parname: string;
  // res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
  // parvalue: string;
  errocode: Integer;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;

  // setting the http return code    request:http_code(404)
  errocode := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtInteger, L,
    -1).AsInteger;
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);

  WebContext := TWebContext(TObject(p));
  WebContext.Response.StatusCode := errocode;
  Result := 0;
end;

function __lua_set_response_headers(L: Plua_State): Integer; cdecl;
var
  parname: string;
  // res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
  parvalue: string;
begin
  if lua_gettop(L) <> 3 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;

  // setting an header  request:headers(name, newvalue)
  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -2).AsString;
  parvalue := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -3, '__self');
  p := lua_topointer(L, -1);

  WebContext := TWebContext(TObject(p));
  WebContext.Response.CustomHeaders.Values[parname] := parvalue;
  Result := 0;
end;

function __lua_post_parameter(L: Plua_State): Integer; cdecl;
var
  parname: string;
  res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  res := WebContext.Request.ContentParam(parname);
  TLuaUtils.PushTValue(L, res);
  Result := 1;
end;

function __lua_set_header_field(L: Plua_State): Integer; cdecl;
var
  parname: string;
  // res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
  parvalue: string;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -2).AsString;
  parvalue := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -3, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  WebContext.Response.SetCustomHeader(parname, parvalue);
  Result := 0;
end;

function __lua_get_parameter(L: Plua_State): Integer; cdecl;
var
  parname: string;
  res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  parname := TLuaValue.GetTValueFromLuaValueType(TLuaValueType.lvtString, L,
    -1).AsString;
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);
  WebContext := TWebContext(TObject(p));
  res := WebContext.Request.QueryStringParam(parname);
  TLuaUtils.PushTValue(L, res);
  Result := 1;
end;

function __lua_stream_out(L: Plua_State): Integer; cdecl;
var
  // parname: string;
  // res: string;
  // rq: TMVCWebRequest;
  p: Pointer;
  WebContext: TWebContext;
  v: string;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit;
  end;
  v := lua_tostring(L, -1);
  lua_getfield(L, -2, '__self');
  p := lua_topointer(L, -1);

  WebContext := TWebContext(TObject(p));
  TStringBuilder(WebContext.ReservedData).Append(v);
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

procedure TMVCEmbeddedLuaView.Execute;
var
  Lua: TLuaEngine;
  k: string;
  LuaFilter: TLuaEmbeddedTextFilter;
  _FFileName, CompiledFileName: string;
  // CompiledTimeStamp   : TDateTime;
  // LuaCode             : string;
  v: string;
  sr: TStreamReader;
  DecodeJSONStrings: string;
  LuaRequestFunctions: TDictionary<string, lua_CFunction>;
  LuaResponseFunctions: TDictionary<string, lua_CFunction>;
  pn: string;
begin
  Lua := TLuaEngine.Create;
  try
    Lua.LoadExternalLibraries(TLuaDataSetExposerLibraries.Create);

    if FFileName.IsEmpty then
    begin // get the real filename from viewname

      FFileName := StringReplace(ViewName, '/', '\', [rfReplaceAll]);
      // $0.02 of normalization
      if FFileName = '\' then
        FFileName := '\index.' + DEFAULT_VIEW_EXT
      else
        FFileName := FFileName + '.' + DEFAULT_VIEW_EXT;

      if DirectoryExists(GetMVCConfig.Value['view_path']) then
        _FFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(GetMVCConfig.Value['view_path']) +
          FFileName)
      else
        _FFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
          GetMVCConfig.Value['view_path']) + FFileName);

      // if not found in the elua folder, find in the document_root
      if not TFile.Exists(_FFileName) then
        FFileName := ExpandFileName
          (IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
          GetMVCConfig.Value['document_root']) + FFileName)
      else
        FFileName := _FFileName;
    end;

    if not FileExists(FileName) then
      raise EMVCFrameworkView.CreateFmt('View [%s.%s] not found',
        [ViewName, DEFAULT_VIEW_EXT])
    else
    begin
      DecodeJSONStrings := '';
      if Assigned(FViewModel) then
        for k in FViewModel.Keys do
          if FViewModel[k] is TJSONValue then
          begin
            Lua.DeclareGlobalString(k, TJSONValue(FViewModel[k]).ToString);
            DecodeJSONStrings := DecodeJSONStrings + sLineBreak + 'local ' +
              AnsiString(k) + ' = json.decode(' + AnsiString(k) + ')';
          end
          else
            Lua.DeclareTable(k, FViewModel[k]);

      if Assigned(FViewDataSets) then
        for k in FViewDataSets.Keys do
          ExposeDataSet(Lua, FViewDataSets[k], FViewDataSets[k].Name);

      for pn in FWebContext.Request.GetParamNames do
      begin
        Lua.DeclareGlobalString(pn, FWebContext.Request.Params[pn]);
      end;

      Lua.DeclareGlobalString('__ROOT__', ExtractFilePath(ParamStr(0)));
      Lua.DeclareGlobalString('__log_file', LOG_FILE_NAME);
      // Lua.DeclareGlobalFunction('__lua_out', @__lua_stream_out);
      Lua.DeclareGlobalLightUserData('__webcontext', WebContext);

      // expose request and response to the LUA engine
      // Lua.DeclareGlobalFunction('request_header', @__lua_request_header);
      // Lua.DeclareGlobalFunction('POST', @__lua_post_parameters);
      // Lua.DeclareGlobalFunction('GET', @__lua_get_parameters);

      LuaResponseFunctions := TDictionary<string, lua_CFunction>.Create;
      try
        LuaResponseFunctions.AddOrSetValue('set_headers',
          @__lua_set_response_headers);
        LuaResponseFunctions.AddOrSetValue('set_http_code',
          @__lua_set_http_code);
        LuaResponseFunctions.AddOrSetValue('out', @__lua_stream_out);
        Lua.DeclareTable('response', WebContext, LuaResponseFunctions);
      finally
        LuaResponseFunctions.Free;
      end;

      LuaRequestFunctions := TDictionary<string, lua_CFunction>.Create;
      try
        LuaRequestFunctions.AddOrSetValue('get', __lua_get_parameter);
        LuaRequestFunctions.AddOrSetValue('post', __lua_post_parameter);
        LuaRequestFunctions.AddOrSetValue('form', __lua_form_parameter);
        LuaRequestFunctions.AddOrSetValue('headers', __lua_headers);
        Lua.DeclareTable('request', WebContext, LuaRequestFunctions);
      finally
        LuaRequestFunctions.Free;
      end;

      CompiledFileName := GetCompiledFileName(FileName);

      TMonitor.Enter(Self);
      try
        if not IsCompiledVersionUpToDate(FileName, CompiledFileName) then
        begin
          LuaFilter := TLuaEmbeddedTextFilter.Create;
          try
            LuaFilter.OutputFunction := '_out';
            LuaFilter.TemplateCode := TFile.ReadAllText(FileName,
              TEncoding.ANSI);
            LuaFilter.Execute;
            TFile.WriteAllText(CompiledFileName, LuaFilter.LuaCode,
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
        fmOpenRead or fmShareDenyNone), TEncoding.ANSI);
      try
        sr.OwnStream;
        v := sr.ReadToEnd;
      finally
        sr.Free;
      end;

      // load lua code
      // Lua.LoadScript(
      // 'require "Lua.helper.view"' + ' require "Lua.delphi.datasets"' + ' json = require "dkjson" ' + DecodeJSONStrings + ' ' + v
      // );

      Lua.LoadScript('require "Lua.boot" ' + DecodeJSONStrings + ' ' + v);

      // prepare to execution and...
      ScriptOutputStringBuilder := TStringBuilder.Create;
      try
        WebContext.ReservedData := ScriptOutputStringBuilder;
        // EXECUTE IT!!!
        Lua.Execute;
        FOutput := ScriptOutputStringBuilder.ToString;
        WebContext.ReservedData := nil;
      finally
        FreeAndNil(ScriptOutputStringBuilder);
        WebContext.ReservedData := nil;
      end;
    end;
  finally
    Lua.Free;
  end;
end;

function TMVCEmbeddedLuaView.GetCompiledFileName(const FileName
  : string): string;
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

function TMVCEmbeddedLuaView.IsCompiledVersionUpToDate(const FileName,
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

procedure TMVCEmbeddedLuaView.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TMVCEmbeddedLuaView.SetOutput(const Value: string);
begin
  FOutput := Value;
end;

{ TMVCBaseView }

constructor TMVCBaseView.Create(AViewName: string; AMVCEngine: TMVCEngine;
  AWebContext: TWebContext; AViewModels: TMVCDataObjects;
  AViewDataSets: TObjectDictionary<string, TDataSet>;
  ACurrentContentType: string);
begin
  inherited Create;
  FViewName := AViewName;
  FWebContext := AWebContext;
  FMVCEngine := AMVCEngine;
  FViewModel := AViewModels;
  FViewDataSets := AViewDataSets;
  // FViewCache := TViewCache.Create;
  FCurrentContentType := ACurrentContentType;
end;

destructor TMVCBaseView.Destroy;
begin
  inherited;
end;

// procedure TMVCBaseView.SetViewCache(const Value: TViewCache);
// begin
// FViewCache := Value;
// end;

end.
