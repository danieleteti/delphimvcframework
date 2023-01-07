// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************
// 
// Original Code has been donated by radek@communicator.pl 
// (https://github.com/danieleteti/delphimvcframework/issues/613#issuecomment-1368555870)
//
// Follows the original comments:
//           Delphi EventSource Client (SSE)              
//                                                        
//                  radek@communicator.pl                 
//                                                        
//  With reference to the specification the only "data"   
//  field of SSE Message if required                      
//
//             !!!!!! Please note !!!!!!
//                                                        
//  Event OnSSEEvent is raised from the thread
//  make sure your handler is thread safe !
//
//  Use OnQueryExtraHeaders to add custom headers such as
//  cookies (Set-Cookie) or
//  Authentication: Bearer XXX.YYY.ZZZ
//
// ENetHTTPResponseException is raised regularly, this is expected bahaviour so recommended
// adding ENetHTTPResponseException to debugger ignored exceptions
// ***************************************************************************
unit MVCFramework.SSEClient;
interface

uses
  System.Net.HttpClient, System.Net.HttpClientComponent, System.SysUtils, System.Net.URLClient, System.Classes, System.Threading;

type
  TOnSSEEvent = procedure(Sender: TObject; const MessageID: integer; const event, data: string) of object;
  TOnQueryExtraHeaders = procedure(Sender: TObject; headers: TURLHeaders) of object;

  TMVCSSEClient = class(TObject)
  private
    fWorkingTask: ITask;
    fLastEventId: integer;
    fReconnectTimeout: integer;
    fEventStream: TStringStream;
    fSSERequest: TNetHTTPClient;
    fURL: string;
    fOnSSEEvent: TOnSSEEvent;
    fOnQueryExtraHeaders: TOnQueryExtraHeaders;
    fTerminated: boolean;
    procedure ReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
  protected
    procedure ExtractMessage(const ASSEMessage: string); virtual;
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;
    property OnSSEEvent: TOnSSEEvent read fOnSSEEvent write fOnSSEEvent;
    property OnQueryExtraHeaders: TOnQueryExtraHeaders read fOnQueryExtraHeaders write fOnQueryExtraHeaders;
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  System.DateUtils;

const
  DefaultReconnectTimeout = 10000;
  CRLF = #13#10;

constructor TMVCSSEClient.Create(const AURL: string);
begin
  inherited Create;
  fTerminated := False;
  fURL := AURL;
  fSSERequest := TNetHTTPClient.Create(nil);
  fSSERequest.ResponseTimeout := 100;
  fSSERequest.Accept := 'text/event-stream';
  fSSERequest.OnReceiveData := ReceiveData;
  fEventStream := TStringStream.Create('', TEncoding.UTF8); ;
  fLastEventId := -1;
end;

destructor TMVCSSEClient.Destroy;
begin
  Stop;

  fSSERequest.Free;
  fEventStream.Free;
  inherited;
end;

procedure TMVCSSEClient.ExtractMessage(const ASSEMessage: string);
var
  SSEMessage: TStrings;
  event, data: string;
begin
  SSEMessage := TStringList.Create;
  SSEMessage.NameValueSeparator := ':';
  try
    SSEMessage.Text := ASSEMessage;

    if SSEMessage.IndexOfName('id')>-1 then
      fLastEventId := SSEMessage.Values['id'].ToInteger;
    if SSEMessage.IndexOfName('event')>-1 then
      event := SSEMessage.Values['event'];
    if SSEMessage.IndexOfName('data')>-1 then
      data := SSEMessage.Values['data'];
    if SSEMessage.IndexOfName('retry')>-1 then
      fReconnectTimeout := StrToIntDef(SSEMessage.Values['retry'], DefaultReconnectTimeout);

    fOnSSEEvent(Self, fLastEventId, event, data);
  finally
    SSEMessage.Free;
  end;
end;

procedure TMVCSSEClient.ReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  lData, lSSEItem: string;
  lSSEItems: TArray<string>;
begin
  AAbort := (fTerminated);
  if not AAbort then
  begin

    lData := fEventStream.DataString.Trim;
    //==============================================================================
    // PARSE THE FOLLOWING:
    //
    //    id: 1
    //    event: sampleEvent
    //    data: testData1
    //    retry: 10000
    //
    //
    //    id: 2
    //    event: sampleEvent
    //    data: testData2
    //    retry: 10000
    //==============================================================================

    lSSEItems := lData.Split([CRLF+CRLF]);
    for lSSEItem in lSSEItems do
      ExtractMessage(lSSEItem);

    fEventStream.Clear;
  end;
end;

procedure TMVCSSEClient.Start;
var
  lNextRetry: TDateTime;
begin
  fReconnectTimeout := DefaultReconnectTimeout;
  if not Assigned(fOnSSEEvent) then
    raise Exception.Create('No event handler defined for OnSSEEvent');

  if Assigned(fOnQueryExtraHeaders) then
    fOnQueryExtraHeaders(Self, fSSERequest.CustHeaders);

  fTerminated := false;

  fWorkingTask := TTask.Run(
  procedure
  begin
    while (not fTerminated) do
    begin
      try
        fSSERequest.CustHeaders.Add('Last-Event-ID', fLastEventId.ToString);
        fSSERequest.Get(FURL, fEventStream);
      except
        on e: exception do
        begin
          if not (e is ENetHTTPResponseException) then
          begin
            //connection to server lost, use fReconnectTimeout
            //non blocking Sleep
            lNextRetry := IncMilliSecond(Now, fReconnectTimeout);
            while Now < lNextRetry do
            begin
              if (fTerminated) then
                Break;
            end;
          end
          else
          //expected read timeout - check if we should leave loop
          if (fTerminated) then
            Break;
        end;
      end;
    end;
  end);
end;

procedure TMVCSSEClient.Stop;
begin
  fTerminated := True;
  if Assigned(fWorkingTask) then
  begin
    TTask.WaitForAll([fWorkingTask]);
  end;
end;

end.
