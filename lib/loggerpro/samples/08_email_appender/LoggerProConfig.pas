unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender,
  LoggerPro.EMailAppender,
  LoggerPro.OutputDebugStringAppender,
  System.SysUtils,
  idSMTP, System.IOUtils,
  IdIOHandlerStack, IdSSL,
  IdSSLOpenSSL, IdExplicitTLSClientServerBase;

var
  _Log: ILogWriter;

const
  USE_SSL = true;

function Log: ILogWriter;
begin
  Result := _Log;
end;

function GetSMTP: TidSMTP;
begin
  Result := TidSMTP.Create(nil);
  try
    if USE_SSL then
    begin
      Result.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
    end;
    Result.Host := 'smtp.gmail.com';
    Result.Port := 25;
    Result.UseTLS := TIdUseTLS.utUseImplicitTLS;
    Result.AuthType := satDefault;
    Result.Username := 'daniele.teti@gmail.com';
    if not TFile.Exists('config.txt') then
      raise Exception.Create('Create a "config.txt" file containing the password');
    Result.Password := TFile.ReadAllText('config.txt'); // '<yourpassword>';
  except
    Result.Free;
    raise;
  end;
end;

procedure SetupLogger;
const

  {$IFDEF DEBUG}

  LOG_LEVEL = TLogType.Debug;

  {$ELSE}

  LOG_LEVEL = TLogType.Warning;

  {$ENDIF}

var
  lEmailAppender: ILogAppender;
begin
  lEmailAppender := TLoggerProEMailAppender.Create(GetSMTP, 'LoggerPro<daniele.teti@gmail.com>', 'd.teti@bittime.it');
  lEmailAppender.SetLogLevel(TLogType.Error);
  _Log := BuildLogWriter([TLoggerProFileAppender.Create,
    lEmailAppender,
    TLoggerProOutputDebugStringAppender.Create], nil, LOG_LEVEL);
end;

initialization

SetupLogger;

end.
