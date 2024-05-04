// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
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

unit LoggerPro.EMailAppender;

interface

uses
  LoggerPro, System.Classes, System.DateUtils,
  idSMTP;

type
  {
    @abstract(Logs sending message to an email address)
    To learn how to use this appender, check the sample @code(email_appender.dproj)
    @author(Daniele Teti - d.teti@bittime.it)
  }
  TLoggerProEMailAppender = class(TLoggerProAppenderBase)
  private
    FSMTP: TIdSMTP;
    FFromAddresses: string;
    FToAddresses: string;
  protected
    /// <summary>
    /// Ovveride this method in descendant if you want a different formatting for
    /// message subject or body
    /// </summary>
    procedure PrepareMessage(const aLogItem: TLogItem; out aSubject, aBody: String); virtual;
  public
    constructor Create(aSMTP: TIdSMTP; const aFromAddresses, aToAddresses: String; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;
  end;

implementation

uses
  System.SysUtils, IdMessage;

constructor TLoggerProEMailAppender.Create(aSMTP: TIdSMTP; const aFromAddresses, aToAddresses: String; aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  FSMTP := aSMTP;
  FFromAddresses := aFromAddresses;
  FToAddresses := aToAddresses;
  { by default, email appender sends only errors }
  SetLogLevel(TLogType.Error);
end;

procedure TLoggerProEMailAppender.PrepareMessage(const aLogItem: TLogItem;
  out aSubject, aBody: String);
begin
  aSubject := 'LoggerPro ' + aLogItem.LogTypeAsString.ToUpper + ' [' + aLogItem.LogTag + ']';
  aBody := FormatLog(aLogItem);
end;

procedure TLoggerProEMailAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProEMailAppender.TearDown;
begin
  try
    FSMTP.Free;
  except
    // do nothing
  end;
end;

procedure TLoggerProEMailAppender.TryToRestart(var Restarted: Boolean);
begin
  inherited;
  try
    FSMTP.Disconnect(false);
  except
  end;
  Restarted := True;
end;

procedure SendEmail(aSMTP: TIdSMTP; const aFromAddresses, aToAddresses: String; const Subject, Body: String);
var
  Msg: TIdMessage;
begin
  Msg := TIdMessage.Create(nil);
  try
    Msg.From.Text := aFromAddresses;
    Msg.Recipients.EMailAddresses := aToAddresses;
    Msg.Body.Text := Body;
    Msg.Subject := Subject;
    if not aSMTP.Connected then
      aSMTP.Connect;
    aSMTP.Send(Msg);
    try
      aSMTP.Disconnect(false);
    except
    end;
  finally
    Msg.Free;
  end;
end;

procedure TLoggerProEMailAppender.WriteLog(const aLogItem: TLogItem);
var
  lBody: string;
  lSubject: string;
begin
  PrepareMessage(aLogItem, lSubject, lBody);
  SendEmail(FSMTP, FFromAddresses, FToAddresses, lSubject, lBody);
end;

end.
