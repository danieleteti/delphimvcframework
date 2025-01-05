// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
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
// *************************************************************************** }

//PS C:\> http --print=hHbB POST :8080/api/shutdown apikey==amoriderediroma
//POST /api/shutdown?apikey=amoriderediroma HTTP/1.1
//Accept: */*
//Accept-Encoding: gzip, deflate
//Connection: keep-alive
//Content-Length: 0
//Host: localhost:8080
//User-Agent: HTTPie/3.2.3
//
//
//
//HTTP/1.1 200 OK
//Connection: keep-alive
//Content-Length: 13
//Content-Type: text/html; charset=utf-8
//Date: Mon, 30 Sep 2024 17:03:13 GMT
//X-Powered-By: DMVCFramework 3.4.2-magnesium-rc3
//
//Shutting down


unit MVCFramework.Middleware.Shutdown;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections;

type
  TMVCShutdownMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fURLSegment: string;
    fQueryStringParamName: string;
    fQueryStringParamValue: string;
  protected
    procedure OnBeforeRouting(aContext: TWebContext; var aHandled: Boolean);
    procedure OnBeforeControllerAction(aContext: TWebContext; const aControllerQualifiedClassName: string;
      const AActionName: string; var aHandled: Boolean);
    procedure OnAfterControllerAction(aContext: TWebContext; const aControllerQualifiedClassName: string;
      const AActionName: string; const aHandled: Boolean);
    procedure OnAfterRouting(aContext: TWebContext; const aHandled: Boolean);
  public
    constructor Create(const aAPIKeyQueryStringParamName, aAPIKeyQueryStringParamValue: String;
      const aURLSegment: String = '/api/shutdown');
    destructor Destroy; override;
    class procedure WaitForShutdownOrConsoleReturn;
  end;

implementation

uses
{$IF Defined(MSWINDOWS)}
  WinAPI.Windows,
{$ENDIF}
  MVCFramework.Logger,
  System.SysUtils,
  System.NetEncoding,
  System.IOUtils,
  System.Classes;

{ TMVCShutdownMiddleware }

constructor TMVCShutdownMiddleware.Create(const aAPIKeyQueryStringParamName, aAPIKeyQueryStringParamValue: String;
  const aURLSegment: String);
begin
  inherited Create;
  fURLSegment := aURLSegment;
  fQueryStringParamName := aAPIKeyQueryStringParamName;
  fQueryStringParamValue := aAPIKeyQueryStringParamValue;
end;

destructor TMVCShutdownMiddleware.Destroy;
begin

  inherited;
end;

procedure TMVCShutdownMiddleware.OnAfterControllerAction(aContext: TWebContext;
  const aControllerQualifiedClassName: string; const AActionName: string; const aHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCShutdownMiddleware.OnAfterRouting(aContext: TWebContext; const aHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCShutdownMiddleware.OnBeforeControllerAction(aContext: TWebContext;
  const aControllerQualifiedClassName, AActionName: string; var aHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCShutdownMiddleware.OnBeforeRouting(aContext: TWebContext; var aHandled: Boolean);
var
  lPathInfo: string;
begin
  lPathInfo := aContext.Request.PathInfo;
  aHandled := False;
  if lPathInfo.StartsWith(fURLSegment, True) and (aContext.Request.HTTPMethod = httpPOST) then
  begin
    if aContext.Request.QueryStringParam(fQueryStringParamName) = fQueryStringParamValue then
    begin
      LogW(ClassName + ' middleware intercepted a shutdown POST request at ' + fURLSegment);
      aContext.Response.Content := 'Shutting down';
      aContext.Response.StatusCode := 200;
      EnterInShutdownState;
      aHandled := True;
    end;
  end;
end;

{$IF Defined(MSWINDOWS)}
function KeyPressed(out KeyPressed: Word): Boolean;
var
  lpNumberOfEvents     : DWORD;
  lpBuffer             : TInputRecord;
  lpNumberOfEventsRead : DWORD;
  nStdHandle           : THandle;
begin
  Result:=false;
  KeyPressed := 0;
  //get the console handle
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents:=0;
  //get the number of events
  GetNumberOfConsoleInputEvents(nStdHandle,lpNumberOfEvents);
  if lpNumberOfEvents<> 0 then
  begin
    //retrieve the event
    PeekConsoleInput(nStdHandle,lpBuffer,1,lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
    begin
      if lpBuffer.EventType = KEY_EVENT then //is a Keyboard event?
      begin
        if lpBuffer.Event.KeyEvent.bKeyDown then //the key was pressed?
        begin
          KeyPressed := lpBuffer.Event.KeyEvent.wVirtualKeyCode;
          Result:=true;
        end;
        FlushConsoleInputBuffer(nStdHandle); //flush the buffer
      end
      else
      begin
        FlushConsoleInputBuffer(nStdHandle);//flush the buffer
      end;
    end;
  end;
end;

class procedure TMVCShutdownMiddleware.WaitForShutdownOrConsoleReturn;
var
  lKeyPressed: Word;
begin
  while True do
  begin
    while (not IsShuttingDown) and (not KeyPressed(lKeyPressed)) do
    begin
      Sleep(500);
    end;
    if IsShuttingDown or (lKeyPressed = VK_RETURN) then
    begin
      LogI('Shutting down...');
      WriteLn('Shutting down...');
      Break;
    end;
  end;
end;
{$ELSE}

class procedure TMVCShutdownMiddleware.WaitForShutdownOrConsoleReturn;
begin
  raise Exception.Create('WaitForShutdownOrConsoleReturn is available only on MSWindows');
end;

{$ENDIF}




end.
