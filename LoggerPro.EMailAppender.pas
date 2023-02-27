unit LoggerPro.EMailAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProRedisAppender))
  @author(Daniele Teti) }

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
    constructor Create(aSMTP: TIdSMTP; const aFromAddresses, aToAddresses: String; aLogFormat: string = DEFAULT_LOG_FORMAT); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;
  end;

implementation

uses
  System.SysUtils, IdMessage;

constructor TLoggerProEMailAppender.Create(aSMTP: TIdSMTP; const aFromAddresses, aToAddresses: String; aLogFormat: string);
begin
  inherited Create(aLogFormat);
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
