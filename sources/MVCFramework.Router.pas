unit MVCFramework.Router;

interface

uses
  Web.HTTPApp,
  RTTIUtilsU,
  MVCFramework.Commons,
  System.RTTI,
  MVCFramework,
  System.Generics.Collections;

type
  TMVCRouter = class
  private
    FMethodToCall: TRTTIMethod;
    FMVCControllerClass: TMVCControllerClass;
    FCTX: TRttiContext;
    FMVCConfig: TMVCConfig;
    function IsHTTPAcceptCompatible(AAccept: AnsiString;
      AAttributes: TArray<TCustomAttribute>): Boolean;

  protected
    function IsHTTPMethodCompatible(AMethodType: TMVCHTTPMethodType;
      AAttributes: TArray<TCustomAttribute>): Boolean; virtual;
    function IsCompatiblePath(AMVCPath: string; APath: string;
      var AParams: TMVCRequestParamsTable): Boolean; virtual;
    function GetAttribute<T: TCustomAttribute>(AAttributes
      : TArray<TCustomAttribute>): T;

  public
    class function StringMethodToHTTPMetod(const Value: AnsiString)
      : TMVCHTTPMethodType;
    constructor Create(AMVCConfig: TMVCConfig);
    function ExecuteRouting(AWebRequest: TWebRequest;
      AMVCControllers: TList<TMVCControllerClass>;
      var AMVCRequestParams: TMVCRequestParamsTable;
      out AResponseContentType, AResponseContentEncoding: string)
      : Boolean; overload;
    function ExecuteRouting(AWebRequestPathInfo: AnsiString;
      AWebRequestMethodType: TMVCHTTPMethodType; AWebRequestAccept: AnsiString;
      AMVCControllers: TList<TMVCControllerClass>;
      var AMVCRequestParams: TMVCRequestParamsTable;
      out AResponseContentType, AResponseContentEncoding: string)
      : Boolean; overload;
    property MethodToCall: TRTTIMethod read FMethodToCall;
    property MVCControllerClass: TMVCControllerClass read FMVCControllerClass;
  end;

implementation

uses
  System.StrUtils,
  System.RegularExpressions,
  System.SysUtils,
  idURI;

{ TMVCRouter }

function TMVCRouter.ExecuteRouting(AWebRequest: TWebRequest;
  AMVCControllers: TList<TMVCControllerClass>;
  var AMVCRequestParams: TMVCRequestParamsTable;
  out AResponseContentType, AResponseContentEncoding: string): Boolean;
var
  HTTPMethodType: TMVCHTTPMethodType;
  controllerClass: TMVCControllerClass;
  _type: TRttiType;
  _methods: TArray<TRTTIMethod>;
  _method: TRTTIMethod;
  _attributes: TArray<TCustomAttribute>;
  _attribute: TCustomAttribute;
  i: Integer;
  ControllerMappedPath: string;
  MethodPathAttribute: string;
  MVCProduceAttr: MVCProducesAttribute;
  AWebRequestPathInfo: AnsiString;
begin
  HTTPMethodType := StringMethodToHTTPMetod(AWebRequest.Method);
  Result := ExecuteRouting(AWebRequest.PathInfo, HTTPMethodType,
    AWebRequest.Accept, AMVCControllers, AMVCRequestParams,
    AResponseContentType, AResponseContentEncoding);
end;

constructor TMVCRouter.Create(AMVCConfig: TMVCConfig);
begin
  inherited Create;
  FMVCConfig := AMVCConfig;
end;

function TMVCRouter.ExecuteRouting(AWebRequestPathInfo: AnsiString;
  AWebRequestMethodType: TMVCHTTPMethodType; AWebRequestAccept: AnsiString;
  AMVCControllers: TList<TMVCControllerClass>;
  var AMVCRequestParams: TMVCRequestParamsTable;
  out AResponseContentType, AResponseContentEncoding: string): Boolean;
var
  controllerClass: TMVCControllerClass;
  _type: TRttiType;
  _methods: TArray<TRTTIMethod>;
  _method: TRTTIMethod;
  _attributes: TArray<TCustomAttribute>;
  _attribute: TCustomAttribute;
  i: Integer;
  ControllerMappedPath: string;
  MethodPathAttribute: string;
  MVCProduceAttr: MVCProducesAttribute;
begin
  FMethodToCall := nil;
  FMVCControllerClass := nil;

  if trim(AWebRequestPathInfo) = EmptyStr then
    AWebRequestPathInfo := '/'
  else
  begin
    if AWebRequestPathInfo[1] <> '/' then
      AWebRequestPathInfo := '/' + AWebRequestPathInfo;
  end;

  // daniele
  AWebRequestPathInfo := TIdURI.URLDecode(AWebRequestPathInfo);

  { ISAPI CHANGE THE REQUEST PATH INFO START }
  if IsLibrary then
  begin
    AWebRequestPathInfo := String(AWebRequestPathInfo).Remove(0, FMVCConfig.Value['isapi_path'].Length);
    if Length(AWebRequestPathInfo) = 0 then
      AWebRequestPathInfo := '/';
  end;
  { ISAPI CHANGE THE REQUEST PATH INFO END }

  Result := False;
  ControllerMappedPath := '';
  for controllerClass in AMVCControllers do
  begin
    _type := FCTX.GetType(controllerClass.ClassInfo);
    _attributes := _type.GetAttributes;
    if not Assigned(_attributes) then
      Continue;

    for _attribute in _attributes do
      if _attribute is MVCPathAttribute then
      begin
        ControllerMappedPath := MVCPathAttribute(_attribute).Path;
        Break;
      end;

    if ControllerMappedPath.IsEmpty then
      raise EMVCException.Create('Controller ' + _type.Name +
        ' doesn''t have MVCPath attribute');

    if ControllerMappedPath = '/' then // WE WANT TO AVOID '//' AS MVCPATH
      ControllerMappedPath := '';

    if (not ControllerMappedPath.IsEmpty) and
      (Pos(ControllerMappedPath, AWebRequestPathInfo) <> 1) then
      Continue;

    _methods := _type.GetMethods;
    for _method in _methods do
    begin
      _attributes := _method.GetAttributes;
      for i := 0 to Length(_attributes) - 1 do
      begin
        _attribute := _attributes[i];
        if _attribute is MVCPathAttribute then
        begin
          if IsHTTPMethodCompatible(AWebRequestMethodType, _attributes) and
            IsHTTPAcceptCompatible(AWebRequestAccept, _attributes) then
          begin
            MethodPathAttribute := MVCPathAttribute(_attribute).Path;
            if IsCompatiblePath(ControllerMappedPath + MethodPathAttribute,
              AWebRequestPathInfo, AMVCRequestParams) then
            begin
              FMethodToCall := _method;
              FMVCControllerClass := controllerClass;
              // getting the default contenttype using MVCProduceAttribute
              MVCProduceAttr := GetAttribute<MVCProducesAttribute>(_attributes);
              if Assigned(MVCProduceAttr) then
              begin
                AResponseContentType := MVCProduceAttr.Value;
                AResponseContentEncoding := MVCProduceAttr.ProduceEncoding;
              end
              else
              begin
                AResponseContentType := TMVCMimeType.APPLICATION_JSON;
                AResponseContentEncoding := 'UTF-8';
              end;

              Exit(true);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TMVCRouter.GetAttribute<T>(AAttributes: TArray<TCustomAttribute>): T;
var
  a: TCustomAttribute;
begin
  Result := nil;
  for a in AAttributes do
    if a is T then
      Exit(T(a));
end;

function TMVCRouter.IsCompatiblePath(AMVCPath: string; APath: string;
  var AParams: TMVCRequestParamsTable): Boolean;
  function ToPattern(const V: string; Names: TList<string>): string;
  var
    s: string;
  begin
    Result := V;
    for s in Names do
      Result := StringReplace(Result, '($' + s + ')',
        '([ אטישעל\.\_\,%\w\d\x2D\x3A]*)', [rfReplaceAll]);
  end;

  function GetParametersNames(const V: string): TList<string>;
  var
    s: string;
    matches: TMatchCollection;
    match: TMatch;
    i: Integer;
  begin
    Result := TList<string>.Create;
    s := '\(\$([A-Za-z0-9]+)\)';
    matches := TRegEx.matches(V, s, [roIgnoreCase, roCompiled, roSingleLine]);
    for match in matches do
      for i := 0 to match.Groups.Count - 1 do
      begin
        s := match.Groups[i].Value;
        if (Length(s) > 0) and (s[1] <> '(') then
        begin
          Result.Add(s);
          Break;
        end;
      end;
  end;

var
  re: TRegEx;
  m: TMatch;
  pattern: string;
  i: Integer;
  Names: TList<string>;
begin
  Names := GetParametersNames(AMVCPath);
  try
    pattern := ToPattern(AMVCPath, Names);
    if APath = AMVCPath then
      Exit(true)
    else
    begin
      re := TRegEx.Create('^' + pattern + '$', [roIgnoreCase, roCompiled,
        roSingleLine]);
      m := re.match(APath);
      Result := m.Success;
      if Result then
        for i := 1 to pred(m.Groups.Count) do
          AParams.Add(Names[i - 1], TIdURI.URLDecode(m.Groups[i].Value));
    end;
  finally
    Names.Free;
  end;
end;

function TMVCRouter.IsHTTPAcceptCompatible(AAccept: AnsiString;
  AAttributes: TArray<TCustomAttribute>): Boolean;
var
  i: Integer;
  Accept: string;
  FoundOneAttribConsumes: Boolean;
begin
  Result := False;
  FoundOneAttribConsumes := False;
  for i := 0 to high(AAttributes) do
  begin
    if AAttributes[i] is MVCConsumesAttribute then
    begin
      FoundOneAttribConsumes := true;
      Accept := MVCConsumesAttribute(AAttributes[i]).Value;
      if Pos(',', AAccept) > 0 then
        AAccept := Copy(AAccept, 1, Pos(',', AAccept) - 1);
      Result := SameText(AAccept, Accept, loInvariantLocale);
    end;
  end;
  Result := (not FoundOneAttribConsumes) or (FoundOneAttribConsumes and Result);
end;

function TMVCRouter.IsHTTPMethodCompatible(AMethodType: TMVCHTTPMethodType;
  AAttributes: TArray<TCustomAttribute>): Boolean;
var
  i: Integer;
  MustBeCompatible: Boolean;
  CompatibleMethods: TMVCHTTPMethods;
begin
  Result := False;
  // if there aren't MVCHTTPMethod attributes defined, the action is compatibile with all methods
  MustBeCompatible := False;
  for i := 0 to high(AAttributes) do
  begin
    if AAttributes[i] is MVCHTTPMethodAttribute then
    begin
      MustBeCompatible := true;
      CompatibleMethods := MVCHTTPMethodAttribute(AAttributes[i])
        .MVCHTTPMethods;
      Result := (AMethodType in CompatibleMethods);
    end;
  end;
  Result := (not MustBeCompatible) or (MustBeCompatible and Result);
end;

class
  function TMVCRouter.StringMethodToHTTPMetod(const Value: AnsiString)
  : TMVCHTTPMethodType;
begin
  if Value = 'GET' then
    Exit(httpGET);
  if Value = 'POST' then
    Exit(httpPOST);
  if Value = 'DELETE' then
    Exit(httpDELETE);
  if Value = 'PUT' then
    Exit(httpPUT);
  if Value = 'HEAD' then
    Exit(httpHEAD);
  if Value = 'OPTIONS' then
    Exit(httpOPTIONS);
  raise EMVCException.CreateFmt('Unknown HTTP method [%s]', [Value]);
end;

end.
