unit MVCFramework.Logger;

interface

uses
  Iocp.Logger,
  System.SysUtils;

type
  TLogLevel = (levNormal = 1, levWar = 2, levError = 3, levException = 4);

function LogLevelAsString(ALogLevel: TLogLevel): string;
procedure Log(AMessage: string); overload;
procedure LogW(AMessage: string);
procedure LogE(AMessage: string);
procedure LogEx(AException: Exception; AMessage: string = '');
procedure Log(LogLevel: TLogLevel; const AMessage: string); overload;
procedure LogEnterMethod(AMethodName: string);
procedure LogExitMethod(AMethodName: string);
procedure LogException(AException: Exception; AMessage: string = '');
  deprecated 'Use LogEx instead';

var
  LogLevelLimit: TLogLevel = TLogLevel.levNormal;

implementation

uses
  System.Classes;

function LogLevelAsString(ALogLevel: TLogLevel): string;
begin
  case ALogLevel of
    levNormal:
      Result := ''; // normal is '' because is more readable
    levWar:
      Result := 'WARNING';
    levError:
      Result := 'ERROR';
    levException:
      Result := 'EXCEPTION';
    else
      Result := 'UNKNOWN';
  end;
end;

procedure LogEx(AException: Exception; AMessage: string = '');
begin
  Log(TLogLevel.levException, Format('[%s] %s (Custom message: "%s")', [AException.ClassName,
    AException.Message, AMessage]));
end;

procedure LogW(AMessage: string);
begin
  Log(TLogLevel.levWar, AMessage);
end;

procedure LogE(AMessage: string);
begin
  Log(TLogLevel.levError, AMessage);
end;

procedure LogException(
  AException: Exception;
  AMessage  : string);
begin
  LogEx(AException, AMessage);
end;

procedure LogEnterMethod(AMethodName: string);
begin
  Log(TLogLevel.levNormal, '>> ' + AMethodName);
end;

procedure LogExitMethod(AMethodName: string);
begin
  Log(TLogLevel.levNormal, '<< ' + AMethodName);
end;

procedure Log(LogLevel: TLogLevel; const AMessage: string);
var
  Msg: string;
begin
  if LogLevel < LogLevelLimit then
    Exit;

  Msg := Format('[%10s %5.5d] %s', [
    LogLevelAsString(LogLevel),
    TThread.CurrentThread.ThreadID,
    AMessage]);

  case LogLevel of
    levNormal:
      AppendLog(Msg, TLogType.ltNormal);
    levWar:
      begin
        AppendLog(Msg, TLogType.ltWarning);
        AppendLog(Msg, TLogType.ltNormal);
      end;
    levError:
      begin
        AppendLog(Msg, TLogType.ltError);
        AppendLog(Msg, TLogType.ltWarning);
        AppendLog(Msg, TLogType.ltNormal);
      end;
    levException:
      begin
        AppendLog(Msg, TLogType.ltException);
        AppendLog(Msg, TLogType.ltError);
        AppendLog(Msg, TLogType.ltWarning);
        AppendLog(Msg, TLogType.ltNormal);
      end
    else
      raise Exception.Create('Invalid LOG LEVEL! Original message was: ' + AMessage);
  end;

end;

procedure Log(AMessage: string); overload;
begin
  Log(TLogLevel.levNormal, AMessage);
end;

end.
