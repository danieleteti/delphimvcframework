{*******************************************************}
{                                                       }
{     Delphi DataSnap Framework Iocp Http WebBroker     }
{                                                       }
{                 soulawing@gmail.com                   }
{                                                       }
{*******************************************************}

unit Iocp.DSHTTPWebBroker;

interface

uses
  Windows, SysUtils, Classes, Types, Math, StrUtils, HTTPApp, WebBroker, WebReq,
  Iocp.HttpServer, Iocp.HttpUtils, Iocp.Utils;

type
  EWBBException = class(Exception);
  EWBBInvalidIdxGetDateVariable = class(EWBBException);
  EWBBInvalidIdxSetDateVariable = class(EWBBException );
  EWBBInvalidIdxGetIntVariable = class(EWBBException );
  EWBBInvalidIdxSetIntVariable = class(EWBBException );
  EWBBInvalidIdxGetStrVariable = class(EWBBException);
  EWBBInvalidIdxSetStringVar = class(EWBBException);
  EWBBInvalidStringVar = class(EWBBException);

  TIocpWebRequest = class(TWebRequest)
  protected
    FHttpConnection: TIocpHttpConnection;
    FContentStream : TStream;
    FFreeContentStream : Boolean;

    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetStringVariable(Index: Integer): AnsiString; override;

    function GetRemoteIP: string; override;
    function GetRawPathInfo: AnsiString; override;
  public
    constructor Create(AHttpConnection: TIocpHttpConnection);
    destructor Destroy; override;
    function GetFieldByName(const Name: AnsiString): AnsiString; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): AnsiString; override;
    {function ReadUnicodeString(Count: Integer): string;}
    function TranslateURI(const URI: string): string; override;
    function WriteClient(var ABuffer; ACount: Integer): Integer; override;

    function WriteHeaders(StatusCode: Integer; const ReasonString, Headers: AnsiString): Boolean; override;
    function WriteString(const AString: AnsiString): Boolean; override;
  end;

  TIocpWebResponse = class(TWebResponse)
  protected
    FHttpConnection: TIocpHttpConnection;
    FContent: AnsiString;
    FStatusCode: Integer;
    FReasonString: string;
    FServer: string;
    FWWWAuthenticate: string;
    FAuthRealm: string;
    FAllow: string;
    FLocation: string;
    FContentEncoding: string;
    FContentType: string;
    FContentVersion: string;
    FDerived_From: string;
    FTitle: string;
    FContentLength: Integer;
    FDate: TDateTime;
    FExpires: TDateTime;
    FLastModified: TDateTime;
    FSent: Boolean;

    function GetContent: AnsiString; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetStatusCode: Integer; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetLogMessage: string; override;
    function GetStringVariable(Index: Integer): AnsiString; override;
    procedure SetContent(const AValue: AnsiString); override;
    procedure SetContentStream(AValue: TStream); override;
    procedure SetStatusCode(AValue: Integer); override;
    procedure SetStringVariable(Index: Integer; const Value: AnsiString); override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
    procedure SetLogMessage(const Value: string); override;
  protected
    function GetResponseHeader: string;
  public
    constructor Create(AHTTPRequest: TWebRequest; AHttpConnection: TIocpHttpConnection);
    procedure SendRedirect(const URI: AnsiString); override;
    procedure SendResponse; override;
    procedure SendStream(AStream: TStream); override;
    function Sent: Boolean; override;
  end;

  TIocpWebBrokerBridge = class(TIocpHttpServer)
  private
    procedure RunWebModuleClass(AHttpConnection: TIocpHttpConnection);
  protected
    FWebModuleClass: TComponentClass;

    procedure DoOnRequest(Client: TIocpHttpConnection); override;
  public
    procedure RegisterWebModuleClass(AClass: TComponentClass);
  end;

implementation

type
  // Make HandleRequest accessible
  TWebDispatcherAccess = class(TCustomWebDispatcher);

const
  INDEX_RESP_Version = 0;
  INDEX_RESP_ReasonString = 1;
  INDEX_RESP_Server = 2;
  INDEX_RESP_WWWAuthenticate = 3;
  INDEX_RESP_Realm = 4;
  INDEX_RESP_Allow = 5;
  INDEX_RESP_Location = 6;
  INDEX_RESP_ContentEncoding = 7;
  INDEX_RESP_ContentType = 8;
  INDEX_RESP_ContentVersion = 9;
  INDEX_RESP_DerivedFrom = 10;
  INDEX_RESP_Title = 11;
  //
  INDEX_RESP_ContentLength = 0;
  //
  INDEX_RESP_Date = 0;
  INDEX_RESP_Expires = 1;
  INDEX_RESP_LastModified = 2;
  //
  //Borland coder didn't define constants in HTTPApp
  INDEX_Method           = 0;
  INDEX_ProtocolVersion  = 1;
  INDEX_URL              = 2;
  INDEX_Query            = 3;
  INDEX_PathInfo         = 4;
  INDEX_PathTranslated   = 5;
  INDEX_CacheControl     = 6;
  INDEX_Date             = 7;
  INDEX_Accept           = 8;
  INDEX_From             = 9;
  INDEX_Host             = 10;
  INDEX_IfModifiedSince  = 11;
  INDEX_Referer          = 12;
  INDEX_UserAgent        = 13;
  INDEX_ContentEncoding  = 14;
  INDEX_ContentType      = 15;
  INDEX_ContentLength    = 16;
  INDEX_ContentVersion   = 17;
  INDEX_DerivedFrom      = 18;
  INDEX_Expires          = 19;
  INDEX_Title            = 20;
  INDEX_RemoteAddr       = 21;
  INDEX_RemoteHost       = 22;
  INDEX_ScriptName       = 23;
  INDEX_ServerPort       = 24;
  INDEX_Content          = 25;
  INDEX_Connection       = 26;
  INDEX_Cookie           = 27;
  INDEX_Authorization    = 28;

{ TIocpWebRequest }

constructor TIocpWebRequest.Create(AHttpConnection: TIocpHttpConnection);
var
  i: Integer;
  LCookies: TStringDynArray;
begin
  FHttpConnection := AHttpConnection;

  inherited Create;

  LCookies := SplitString(FHttpConnection.RequestCookies, ';'+#0);
  for i := Low(LCookies) to High(LCookies) do
    CookieFields.Add(LCookies[i]);

  if (FHttpConnection.RequestPostData.Size > 0) then
  begin
    FContentStream := FHttpConnection.RequestPostData;
    FFreeContentStream := False;
  end;
end;

destructor TIocpWebRequest.Destroy;
begin
  if FFreeContentStream then begin
    FreeAndNil(FContentStream);
  end;
  inherited;
end;

function TIocpWebRequest.GetDateVariable(Index: Integer): TDateTime;
var
  LValue: string;
begin
  LValue := string(GetStringVariable(Index));
  if Length(LValue) > 0 then begin
    Result := ParseDate(LValue);
  end else begin
    Result := -1;
  end;
end;

function TIocpWebRequest.GetIntegerVariable(Index: Integer): Integer;
begin
  Result := StrToIntDef(string(GetStringVariable(Index)), -1)
end;

function TIocpWebRequest.GetRawPathInfo: AnsiString;
begin
  Result := AnsiString(FHttpConnection.RawPath);
end;

function TIocpWebRequest.GetRemoteIP: string;
begin
  Result := FHttpConnection.PeerIP;
end;

function TIocpWebRequest.GetStringVariable(Index: Integer): AnsiString;
begin
  case Index of
    INDEX_Method          : Result := AnsiString(FHttpConnection.Method);
    INDEX_ProtocolVersion : Result := AnsiString(FHttpConnection.Version);
    //INDEX_URL             : Result := AnsiString(FRequestInfo.Document);
    INDEX_URL             : Result := AnsiString(''); // Root - consistent with ISAPI which return path to root
    INDEX_Query           : Result := AnsiString(FHttpConnection.Params);
    INDEX_PathInfo        : Result := AnsiString(FHttpConnection.Path);
    INDEX_PathTranslated  : Result := AnsiString(FHttpConnection.Path);             // it's not clear quite what should be done here - we can't translate to a path
    INDEX_CacheControl    : Result := AnsiString(GetFieldByName('Cache-Control'));   {do not localize}
    INDEX_Date            : Result := AnsiString(GetFieldByName('Date'));            {do not localize}
    INDEX_Accept          : Result := AnsiString(FHttpConnection.RequestAccept);
    INDEX_From            : Result := AnsiString(GetFieldByName('From'));
    INDEX_Host            : Result := AnsiString(FHttpConnection.RequestHostName);
    INDEX_IfModifiedSince : Result := AnsiString(GetFieldByName('If-Modified-Since')); {do not localize}
    INDEX_Referer         : Result := AnsiString(FHttpConnection.RequestReferer);
    INDEX_UserAgent       : Result := AnsiString(FHttpConnection.RequestUserAgent);
    INDEX_ContentEncoding : Result := AnsiString(GetFieldByName('Content-Encoding'));
    INDEX_ContentType     : Result := AnsiString(FHttpConnection.RequestContentType);
    INDEX_ContentLength   : Result := AnsiString(IntToStr(FHttpConnection.RequestContentLength));
    INDEX_ContentVersion  : Result := AnsiString(GetFieldByName('CONTENT_VERSION')); {do not localize}
    INDEX_DerivedFrom     : Result := AnsiString(GetFieldByName('Derived-From'));    {do not localize}
    INDEX_Expires         : Result := AnsiString(GetFieldByName('Expires'));         {do not localize}
    INDEX_Title           : Result := AnsiString(GetFieldByName('Title'));           {do not localize}
    INDEX_RemoteAddr      : Result := AnsiString(FHttpConnection.PeerIP);
    INDEX_RemoteHost      : Result := AnsiString(GetFieldByName('REMOTE_HOST'));     {do not localize}
    INDEX_ScriptName      : Result := AnsiString('');
    INDEX_ServerPort      : Result := AnsiString(FHttpConnection.RequestHostPort);
    INDEX_Content: begin
      if FFreeContentStream then
      begin
        Result := AnsiString(TStringStream(FContentStream).DataString);
      end else
      begin
        Result := FHttpConnection.RequestPostData.DataString;
      end;
    end;
    INDEX_Connection      : Result := AnsiString(FHttpConnection.RequestConnection);      {do not localize}
    INDEX_Cookie          : Result := AnsiString('');  // not available at present. FRequestInfo.Cookies....;
    INDEX_Authorization   : Result := AnsiString(GetFieldByName('Authorization'));   {do not localize}
  else
    Result := '';
  end;
end;

function TIocpWebRequest.GetFieldByName(const Name: AnsiString): AnsiString;
begin
  Result := AnsiString(FHttpConnection.GetRequestField(string(Name)));
end;

function TIocpWebRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := FContentStream.Read(Buffer, Count);
  if (Result < 0) then
    Result := 0;
end;

function TIocpWebRequest.ReadString(Count: Integer): AnsiString;
begin
  SetLength(Result, Count);
  FContentStream.Read(Result[1], Count);
  SetLength(Result, StrLen(PAnsiChar(Result)));
end;

function TIocpWebRequest.TranslateURI(const URI: string): string;
begin
  // we don't have the concept of a path translation. It's not quite clear
  // what to do about this. Comments welcome (grahame@kestral.com.au)
  Result := URI;
end;

function TIocpWebRequest.WriteHeaders(StatusCode: Integer; const ReasonString, Headers: AnsiString): Boolean;
begin
{  FResponseInfo.ResponseNo := StatusCode;
  FResponseInfo.ResponseText := string(ReasonString);
  FResponseInfo.CustomHeaders.Add(string(Headers));
  FResponseInfo.WriteHeader;
  LHeader := ''
  FHttpConnection.AnswerHTML();}
  Result := True;
end;

function TIocpWebRequest.WriteString(const AString: AnsiString): Boolean;
begin
  Result := FHttpConnection.AnswerHTML('', AString);
end;

function TIocpWebRequest.WriteClient(var ABuffer; ACount: Integer): Integer;
begin
  Result := FHttpConnection.Send(@ABuffer, ACount);
end;

{ TIocpWebResponse }

constructor TIocpWebResponse.Create(AHTTPRequest: TWebRequest; AHttpConnection: TIocpHttpConnection);
begin
  FHttpConnection := AHttpConnection;

  inherited Create(AHTTPRequest);
  if Length(FHTTPRequest.ProtocolVersion) = 0 then begin
    Version := '1.0'; {do not localize}
  end;
  StatusCode := 200;
  LastModified := -1;
  Expires := -1;
  Date := -1;
  ContentType := 'text/html'; {do not localize}
end;

function TIocpWebResponse.GetContent: AnsiString;
begin
  Result := FContent;
end;

function TIocpWebResponse.GetLogMessage: string;
begin
  Result := '';
end;

function TIocpWebResponse.GetResponseHeader: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Cookies.Count - 1 do
  begin
    Result := 'Set-Cookie: ' + string(Cookies[i].HeaderValue) + #13#10;
  end;
  AddCustomHeaders(Result);
end;

function TIocpWebResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function OffsetFromUTC: TDateTime;
{$IFDEF DOTNET}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
var
  iBias: Integer;
  tmez: TTimeZoneInformation;
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF USE_VCL_POSIX}
var
  T : Time_t;
  TV : TimeVal;
  UT : tm;
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
 var
   timeval: TTimeVal;
   timezone: TTimeZone;
    {$ENDIF}
    {$IFDEF KYLIXCOMPAT}
var
  T: Time_T;
  TV: TTimeVal;
  UT: TUnixTime;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF LINUX}

    {$IFDEF USE_VCL_POSIX}
  {from http://edn.embarcadero.com/article/27890 }

  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(T, UT);
  Result := -1*(UT.tm_gmtoff / 60 / 60 / 24);
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
  fpGetTimeOfDay (@TimeVal, @TimeZone);
  Result := -1 * (timezone.tz_minuteswest /60 / 60 / 24)
    {$ENDIF}
    {$IFDEF KYLIXCOMPAT}
  {from http://edn.embarcadero.com/article/27890 }

  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(@T, UT);
  Result := -1*(UT.__tm_gmtoff / 60 / 60 / 24);
    {$ENDIF}
    // __tm_gmtoff is the bias in seconds from the UTC to the current time.
    // so I multiply by -1 to compensate for this.

  {$ENDIF}
  {$IFDEF DOTNET}
  Result := System.Timezone.CurrentTimezone.GetUTCOffset(DateTime.FromOADate(Now)).TotalDays;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  case GetTimeZoneInformation(tmez) of
    TIME_ZONE_ID_INVALID  :
      raise Exception.Create('Failed attempting to retrieve time zone information.');
    TIME_ZONE_ID_UNKNOWN  :
       iBias := tmez.Bias;
    TIME_ZONE_ID_DAYLIGHT : begin
      iBias := tmez.Bias;
      if tmez.DaylightDate.wMonth <> 0 then begin
        iBias := iBias + tmez.DaylightBias;
      end;
    end;
    TIME_ZONE_ID_STANDARD : begin
      iBias := tmez.Bias;
      if tmez.StandardDate.wMonth <> 0 then begin
        iBias := iBias + tmez.StandardBias;
      end;
    end
  else
    begin
      raise Exception.Create('Failed attempting to retrieve time zone information.');
    end;
  end;
  {We use ABS because EncodeTime will only accept positive values}
  Result := EncodeTime(Abs(iBias) div 60, Abs(iBias) mod 60, 0, 0);
  {The GetTimeZone function returns values oriented towards converting
   a GMT time into a local time.  We wish to do the opposite by returning
   the difference between the local time and GMT.  So I just make a positive
   value negative and leave a negative value as positive}
  if iBias > 0 then begin
    Result := 0.0 - Result;
  end;
  {$ENDIF}
end;

function TIocpWebResponse.GetDateVariable(Index: Integer): TDateTime;
  // WebBroker apps are responsible for conversion to GMT, Indy HTTP server expects apps to pas local time
  function ToGMT(ADateTime: TDateTime): TDateTime;
  begin
    Result := ADateTime;
    if Result <> -1 then
      Result := Result - OffsetFromUTC;
  end;
begin
  case Index of
    INDEX_RESP_Date             : Result := ToGMT(FDate);
    INDEX_RESP_Expires          : Result := ToGMT(FExpires);
    INDEX_RESP_LastModified     : Result := ToGMT(FLastModified);
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

procedure TIocpWebResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
  // WebBroker apps are responsible for conversion to GMT, Indy HTTP server expects apps to pas local time
  function ToLocal(ADateTime: TDateTime): TDateTime;
  begin
    Result := ADateTime;
    if Result <> -1 then
      Result := Result + OffsetFromUTC;
  end;
begin
  case Index of
    INDEX_RESP_Date             : FDate := ToLocal(Value);
    INDEX_RESP_Expires          : FExpires := ToLocal(Value);
    INDEX_RESP_LastModified     : FLastModified := ToLocal(Value);
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

function TIocpWebResponse.GetIntegerVariable(Index: Integer): Integer;
begin

  case Index of
    INDEX_RESP_ContentLength: Result := FContentLength;
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

procedure TIocpWebResponse.SetIntegerVariable(Index, Value: Integer);
begin

  case Index of
    INDEX_RESP_ContentLength: FContentLength := Value;
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

function TIocpWebResponse.GetStringVariable(Index: Integer): AnsiString;
begin

  case Index of
    INDEX_RESP_Version           :Result := AnsiString(FHttpConnection.Version);
    INDEX_RESP_ReasonString      :Result := AnsiString(FReasonString);
    INDEX_RESP_Server            :Result := AnsiString(FServer);
    INDEX_RESP_WWWAuthenticate   :Result := AnsiString(FWWWAuthenticate);
    INDEX_RESP_Realm             :Result := AnsiString(FAuthRealm);
    INDEX_RESP_Allow             :Result := AnsiString(FAllow);        {do not localize}
    INDEX_RESP_Location          :Result := AnsiString(FLocation);
    INDEX_RESP_ContentEncoding   :Result := AnsiString(FContentEncoding);
    INDEX_RESP_ContentType       :Result := AnsiString(FContentType);
    INDEX_RESP_ContentVersion    :Result := AnsiString(FContentVersion);
    INDEX_RESP_DerivedFrom       :Result := AnsiString(FDerived_From); {do not localize}
    INDEX_RESP_Title             :Result := AnsiString(FTitle);        {do not localize}
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

procedure TIocpWebResponse.SetStringVariable(Index: Integer; const Value: AnsiString);
begin

  case Index of
    INDEX_RESP_Version           : EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
    INDEX_RESP_ReasonString      : FReasonString := string(Value);
    INDEX_RESP_Server            : FServer := string(Value);
    INDEX_RESP_WWWAuthenticate   : FWWWAuthenticate := string(Value);
    INDEX_RESP_Realm             : FAuthRealm := string(Value);
    INDEX_RESP_Allow             : FAllow := string(Value); {do not localize}
    INDEX_RESP_Location          : FLocation := string(Value);
    INDEX_RESP_ContentEncoding   : FContentEncoding := string(Value);
    INDEX_RESP_ContentType       : FContentType := string(Value);
    INDEX_RESP_ContentVersion    : FContentVersion := string(Value);
    INDEX_RESP_DerivedFrom       : FDerived_From := string(Value);  {do not localize}
    INDEX_RESP_Title             : FTitle := string(Value); {do not localize}
  else
    raise EWBBInvalidStringVar.CreateFmt('Invalid Index %d in TIocpWebResponse.SetStringVariable', [Index]);
  end;
end;

procedure TIocpWebResponse.SendRedirect(const URI: AnsiString);
begin
  SetCustomHeader('Location', string(URI));
  SendResponse;
end;

procedure TIocpWebResponse.SendResponse;
  function ReasonStr: string;
  begin
    Result := string(ReasonString);
    if (Result = '') then
      Result := string(StatusString(StatusCode));
  end;
var
  LStatusStr: string;
begin
  LStatusStr := IntToStr(StatusCode) + ' ' + ReasonStr;
  if (FContent <> '') then
    FSent := FHttpConnection.AnswerHTML(LStatusStr, string(ContentType), GetResponseHeader, FContent)
  else if Assigned(ContentStream) and (ContentStream.Size > 0) then
    FSent := FHttpConnection.AnswerStream(LStatusStr, string(ContentType), GetResponseHeader, ContentStream)
  else
    FSent := FHttpConnection.AnswerHTML(LStatusStr, string(ContentType), GetResponseHeader,
      '<HTML><BODY><B>' + LStatusStr + '</B></BODY></HTML>')
end;

procedure TIocpWebResponse.SendStream(AStream: TStream);
begin
  FHttpConnection.Send(AStream);
end;

function TIocpWebResponse.Sent: Boolean;
begin
  Result := FSent;
end;

procedure TIocpWebResponse.SetContent(const AValue: AnsiString);
begin
  FContent := AValue;
end;

procedure TIocpWebResponse.SetLogMessage(const Value: string);
begin
  // logging not supported
end;

procedure TIocpWebResponse.SetStatusCode(AValue: Integer);
begin
  FStatusCode := AValue;
end;

procedure TIocpWebResponse.SetContentStream(AValue: TStream);
begin
  inherited;
//  FResponseInfo.ContentStream := AValue;
end;

{ TIocpWebBrokerBridge }

type
  TIocpWebBrokerBridgeRequestHandler = class(TWebRequestHandler)
  {$IFDEF HAS_CLASSVARS}
  private class var
    FWebRequestHandler: TIocpWebBrokerBridgeRequestHandler;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF HAS_CLASSVARS}
      {$IFDEF HAS_CLASSDESTRUCTOR}
    class destructor Destroy;
      {$ENDIF}
    {$ENDIF}
    destructor Destroy; override;
    procedure Run(AHttpConnection: TIocpHttpConnection);
  end;

{$IFNDEF HAS_CLASSVARS}
var
  IocpWebRequestHandler: TIocpWebBrokerBridgeRequestHandler = nil;
{$ENDIF}

{ TIocpWebBrokerBridgeRequestHandler }

procedure TIocpWebBrokerBridgeRequestHandler.Run(AHttpConnection: TIocpHttpConnection);
var
  LRequest: TIocpWebRequest;
  LResponse: TIocpWebResponse;
begin
  try
    LRequest := TIocpWebRequest.Create(AHttpConnection);
    try
      LResponse := TIocpWebResponse.Create(LRequest, AHttpConnection);
      try
        HandleRequest(LRequest, LResponse);
      finally
        FreeAndNil(LResponse);
      end;
    finally
      FreeAndNil(LRequest);
    end;
  except
    // Let Indy handle this exception
    raise;
  end;
end;

constructor TIocpWebBrokerBridgeRequestHandler.Create(AOwner: TComponent);
begin
  inherited;
  Classes.ApplicationHandleException := HandleException;
end;

destructor TIocpWebBrokerBridgeRequestHandler.Destroy;
begin
  Classes.ApplicationHandleException := nil;
  inherited;
end;

{$IFDEF HAS_CLASSVARS}
  {$IFDEF HAS_CLASSDESTRUCTOR}
class destructor TIocpWebBrokerBridgeRequestHandler.Destroy;
begin
  FreeAndNil(FWebRequestHandler);
end;
  {$ENDIF}
{$ENDIF}

function IocpWebBrokerBridgeRequestHandler: TWebRequestHandler;
begin
  {$IFDEF HAS_CLASSVARS}
  if not Assigned(TIocpWebBrokerBridgeRequestHandler.FWebRequestHandler) then
    TIocpWebBrokerBridgeRequestHandler.FWebRequestHandler := TIocpWebBrokerBridgeRequestHandler.Create(nil);
  Result := TIocpWebBrokerBridgeRequestHandler.FWebRequestHandler;
  {$ELSE}
  if not Assigned(IocpWebRequestHandler) then
    IocpWebRequestHandler := TIocpWebBrokerBridgeRequestHandler.Create(nil);
  Result := IocpWebRequestHandler;
  {$ENDIF}
end;

procedure TIocpWebBrokerBridge.DoOnRequest(Client: TIocpHttpConnection);
begin
  try
    if FWebModuleClass <> nil then
    begin
      // FWebModuleClass, RegisterWebModuleClass supported for backward compatability
      RunWebModuleClass(Client)
    end else
    begin
      {$IFDEF HAS_CLASSVARS}
      TIocpWebBrokerBridgeRequestHandler.FWebRequestHandler.Run(Client);
      {$ELSE}
      IocpWebRequestHandler.Run(Client);
      {$ENDIF}
    end;
  except
    Client.Disconnect;
  end;
end;

procedure TIocpWebBrokerBridge.RunWebModuleClass(AHttpConnection: TIocpHttpConnection);
var
  LRequest: TIocpWebRequest;
  LResponse: TIocpWebResponse;
  LWebModule: TCustomWebDispatcher;
  {$IFDEF VCL_6_OR_ABOVE}
  WebRequestHandler: IWebRequestHandler;
  {$ENDIF}
  Handled: Boolean;
begin
  LRequest := TIocpWebRequest.Create(AHttpConnection);
  try
    LResponse := TIocpWebResponse.Create(LRequest, AHttpConnection);
    try
      // There are better ways in D6, but this works in D5
      LWebModule := FWebModuleClass.Create(nil) as TCustomWebDispatcher;
      try
        {$IFDEF VCL_6_OR_ABOVE}
        if Supports(LWebModule, IWebRequestHandler, WebRequestHandler) then begin
          try
            Handled := WebRequestHandler.HandleRequest(LRequest, LResponse);
          finally
            WebRequestHandler := nil;
          end;
        end else begin
          Handled := TWebDispatcherAccess(LWebModule).DispatchAction(LRequest, LResponse);
        end;
        {$ELSE}
        Handled := TWebDispatcherAccess(LWebModule).DispatchAction(LRequest, LResponse);
        {$ENDIF}
        if Handled and (not LResponse.Sent) then begin
          LResponse.SendResponse;
        end;
      finally
        FreeAndNil(LWebModule);
      end;
    finally
      FreeAndNil(LResponse);
    end;
  finally
    FreeAndNil(LRequest);
  end;
end;

// FWebModuleClass, RegisterWebModuleClass supported for backward compatability
// Instead set WebModuleClass using: WebReq.WebRequestHandler.WebModuleClass := TWebModule1;
procedure TIocpWebBrokerBridge.RegisterWebModuleClass(AClass: TComponentClass);
begin
  FWebModuleClass := AClass;
end;

initialization
  WebReq.WebRequestHandlerProc := IocpWebBrokerBridgeRequestHandler;
{$IFDEF HAS_CLASSVARS}
  {$IFNDEF HAS_CLASSDESTRUCTOR}
finalization
  FreeAndNil(TIocpWebBrokerBridgeRequestHandler.FWebRequestHandler);
  {$ENDIF}
{$ELSE}
finalization
  FreeAndNil(IocpWebRequestHandler);
{$ENDIF}

end.

