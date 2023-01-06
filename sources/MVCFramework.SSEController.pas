// *************************************************************************** }
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

unit MVCFramework.SSEController;

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections;

type
  TMVCSSEDefaults = class sealed
  public const
    /// <summary>
    /// Charset of SSE messages encoding
    /// </summary>
    SSE_CONTENT_CHARSET = TMVCConstants.DEFAULT_CONTENT_CHARSET;
    /// <summary>
    /// Keep the messages in correct order and resend lost messages after reconnection
    ///
    /// Hint:
    /// If you don't case about message order (i.e. raising current time, current temperature) leave it false
    /// If you need to push events without data loss and in the right order use True (i.e. bank transactions)
    /// </summary>
    SSE_KEEP_ORDER = false;
  end;

  TSSEMessage = record
    Event: string;
    Data: string;
    Id: String;
    Retry: Integer;
  end;

  TMVCSSEMessages = TArray<TSSEMessage>;

  TMVCSSEController = class abstract(TMVCController)
  protected
    fSSECharset: string;
    /// <summary>
    /// Overwrite this method in inherited class !
    /// </summary>
    function GetServerSentEvents(const LastEventID: String): TMVCSSEMessages; virtual; abstract;
  public
    constructor Create(const ASSECharset: string = TMVCSSEDefaults.SSE_CONTENT_CHARSET); reintroduce; overload;
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure Index;
  end;

implementation

uses
  IdContext, IdHTTPWebBrokerBridge, IdIOHandler;

constructor TMVCSSEController.Create(const ASSECharset: string);
begin
  inherited Create;
  fSSECharset := ASSECharset;
end;

type
  TIdHTTPAppResponseAccess = class(TIdHTTPAppResponse);

procedure TMVCSSEController.Index;
var
  lRawContext: TIdContext;
  lDataList: TMVCSSEMessages;
  lSSEData: TSSEMessage;
  lLastEventID: String;
  lIOHandler: TIdIOHandler;
const
  EOL = #13#10;
begin
  inherited;
  if not (Context.Response.RawWebResponse is TIdHTTPAppResponse) then
  begin
    raise EMVCException.Create(HTTP_STATUS.InternalServerError, ClassName + ' can only be used with INDY based application server');
  end;

  lRawContext := TIdHTTPAppResponseAccess(Context.Response.RawWebResponse).FThread;

  lLastEventID := Context.Request.Headers[TMVCConstants.SSE_LAST_EVENT_ID].Trim;

  lIOHandler := lRawContext.Connection.IOHandler;
  lIOHandler.WriteBufferOpen();
  lIOHandler.WriteLn('HTTP/1.1 200 OK');
  lIOHandler.WriteLn(Format('Content-Type: text/event-stream; charset=%s', [fSSECharset]));
  lIOHandler.WriteLn('Cache-Control: no-cache');
  lIOHandler.WriteLn('Connection: keep-alive');

  {TODO -oDanieleT -cSSE : We must handle CORS using constructor parameters}
  lIOHandler.WriteLn('Access-Control-Allow-Origin: *');
  lIOHandler.WriteLn('Access-Control-Allow-Methods: POST, PUT, DELETE, GET, OPTIONS');
  lIOHandler.WriteLn('Access-Control-Request-Method: *');
  lIOHandler.WriteLn('Access-Control-Allow-Headers: Origin, X-Requested-With, Content-Type, Accept, Authorization');

  lIOHandler.WriteLn;
  lIOHandler.WriteBufferClose;

  while lRawContext.Connection.Connected do
  begin
    lDataList := [];
    // query for next data list
    lDataList := GetServerSentEvents(lLastEventID);

    if (Length(lDataList) > 0) then
    begin
      lIOHandler.WriteBufferOpen;
      for lSSEData in lDataList do
      begin
        if not lSSEData.Id.IsEmpty then
        begin
          lIOHandler.Write(Format('id: %s' + EOL, [lSSEData.Id]));
          lLastEventID := lSSEData.Id;
        end;
        if not lSSEData.Event.IsEmpty then
        begin
          lIOHandler.Write(Format('event: %s' + EOL, [lSSEData.Event]));
        end;
        lIOHandler.Write(Format('data: %s' + EOL + EOL { end of message } , [lSSEData.Data]));
      end;
      lIOHandler.WriteBufferClose;
    end;
    Sleep(200); //arbitrary... some better approches?
  end;
  lRawContext.Connection.Disconnect;
end;

end.
