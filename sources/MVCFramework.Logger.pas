{ *************************************************************************** }
{ }
{ Delphi MVC Framework }
{ }
{ Copyright (c) 2010-2015 Daniele Teti and the DMVCFramework Team }
{ }
{ https://github.com/danieleteti/delphimvcframework }
{ }
{ *************************************************************************** }
{ }
{ Licensed under the Apache License, Version 2.0 (the "License"); }
{ you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at }
{ }
{ http://www.apache.org/licenses/LICENSE-2.0 }
{ }
{ Unless required by applicable law or agreed to in writing, software }
{ distributed under the License is distributed on an "AS IS" BASIS, }
{ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{ See the License for the specific language governing permissions and }
{ limitations under the License. }
{ }
{ *************************************************************************** }

unit MVCFramework.Logger;

interface

{$DEFINE MVC_LOGENABLED}

uses
{$IFDEF MVC_LOGENABLED}
  Iocp.Logger,
{$ELSE}
  // Allow different log library.
{$ENDIF}
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
  Log(TLogLevel.levException, Format('[%s] %s (Custom message: "%s")',
    [AException.ClassName, AException.Message, AMessage]));
end;

procedure LogW(AMessage: string);
begin
  Log(TLogLevel.levWar, AMessage);
end;

procedure LogE(AMessage: string);
begin
  Log(TLogLevel.levError, AMessage);
end;

procedure LogException(AException: Exception; AMessage: string);
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

{$IFDEF MVC_LOGENABLED}

procedure Log(LogLevel: TLogLevel; const AMessage: string);
var
  Msg: string;
begin
  if LogLevel < LogLevelLimit then
    Exit;

  Msg := Format('[%10s %5.5d] %s', [LogLevelAsString(LogLevel),
    TThread.CurrentThread.ThreadID, AMessage]);

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
    raise Exception.Create('Invalid LOG LEVEL! Original message was: ' +
      AMessage);
  end;
end;
{$ELSE}

procedure Log(LogLevel: TLogLevel; const AMessage: string);
begin
 // Custom logging
end;
{$ENDIF}

procedure Log(AMessage: string); overload;
begin
  Log(TLogLevel.levNormal, AMessage);
end;

end.
