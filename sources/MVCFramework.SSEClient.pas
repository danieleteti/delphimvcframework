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
//  If you want to use https put the                      
//  libeay32.dll and ssleay32.dll (64 or 32 bit)          
//  in the exe folder                                     
//                                                        
//             !!!!!! Please note !!!!!!                  
//                                                        
//  Event OnSSEEvent is raised from the thread            
//  make sure you handler is thread safe !                
//                                                        
//  Use OnQueryExtraHeaders to add custom headers such as 
//  cookies (Set-Cookie) or                               
//  Authentication: Bearer XXX.YYY.ZZZ     
//  
// ***************************************************************************

unit MVCFramework.SSEClient;

interface

uses
  IdHTTP, IdGlobal, System.SysUtils, IdSSLOpenSSL,
  System.Classes, System.Threading, IdComponent;

type
  TOnSSEEvent = procedure(Sender: TObject; const MessageID: Integer; const Event, Data: string) of object;
  TOnQueryExtraHeaders = procedure(Sender: TObject; Headers: TStrings) of object;

  TMVCSSEClient = class(TObject)
  private
    fWorkingTask: ITask;
    fLastEventId: integer;
    fReconnectTimeout: integer;
    fEventStream: TIdEventStream;
    fIdHTTP: TIdHTTP;
    fIdSSL: TIdSSLIOHandlerSocketOpenSSL;
    fURL: string;
    fOnSSEEvent: TOnSSEEvent;
    fOnQueryExtraHeaders: TOnQueryExtraHeaders;
    fTerminated: Boolean;
  protected
    procedure DataAvailable(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
    procedure ExtractMessage(const ASSEMessage: string); virtual;
    procedure OnSSEWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;
    property OnSSEEvent: TOnSSEEvent read FOnSSEEvent write FOnSSEEvent;
    property OnQueryExtraHeaders: TOnQueryExtraHeaders read FOnQueryExtraHeaders write FOnQueryExtraHeaders;
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
  fIdHTTP := TIdHTTP.Create(nil);

  fIdSSL := nil;
  if AURL.ToLower.StartsWith('https') then
  begin
    fIdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    fIdSSL.SSLOptions.Method := sslvSSLv23; //wildcard for accepting all versions
    fIdHTTP.IOHandler := fIdSSL;
  end;
  fIdHTTP.Request.Accept := 'text/event-stream';
  fIdHTTP.Request.CacheControl := 'no-store';

  fEventStream := TIdEventStream.Create;
  fEventStream.OnWrite := DataAvailable;

  fLastEventId := -1;
end;

destructor TMVCSSEClient.Destroy;
begin
  Stop;

  fIdHTTP.Free;
  fIdSSL.Free;
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

procedure TMVCSSEClient.OnSSEWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if fTerminated then
  begin
    IndyRaiseLastError;
  end;
end;

procedure TMVCSSEClient.DataAvailable(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
var
  lData: string;
  lSSEItems: TArray<string>;
  lSSEItem: string;
begin
  lData := IndyTextEncoding_UTF8.GetString(ABuffer).Trim;

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

end;

procedure TMVCSSEClient.Start;
var
  lNextRetry: TDateTime;
begin
  fReconnectTimeout := DefaultReconnectTimeout;
  if not Assigned(fOnSSEEvent) then
    raise Exception.Create('No event handler defined for OnSSEEvent');

  if Assigned(FOnQueryExtraHeaders) then
    fOnQueryExtraHeaders(Self, fIdHTTP.Request.CustomHeaders);

  fWorkingTask := TTask.Run(
  procedure
  begin
    //while (fWorkingTask.Status = TTaskStatus.Running) do
    while not fTerminated do
    begin
      try
        fIdHTTP.Request.CustomHeaders.AddValue('Last-Event-ID', fLastEventId.ToString);
        fIdHTTP.OnWork := OnSSEWork;
        fIdHTTP.Get(FURL, fEventStream);
      except
        //non blocking Sleep
        lNextRetry := IncMilliSecond(Now, fReconnectTimeout);
        while Now < lNextRetry do
        begin
          if fWorkingTask.Status <> TTaskStatus.Running then
            Break;
          TThread.Yield;
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
//    fWorkingTask.Cancel;
    TTask.WaitForAll([fWorkingTask]); //this never returns...
  end;
  fIdHTTP.Disconnect;
end;

end.
