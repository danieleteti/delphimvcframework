#ifndef UnitSetupUtils
#define UnitSetupUtils

[Code]
{************************************************************************}
{                                                                        }
{ InnoSetup Tools Library for Delphi Components                          }
{                                                                        }
{ Copyright (c) 2024-2025 Ethea S.r.l.                                   }
{                                                                        }
{ Original Code is Copyright (c) 2021-2025 Skia4Delphi Project.          }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
// unit Setup.Utils;

// interface

/// <summary> Check if the setup was executed with an specific param </summary>
function HasParam(const AParamName: string): Boolean; forward;
/// <summary> Check if an app is running by its file name, without path, like 'bds.exe' </summary>
function IsAppRunning(const AFileName: string): Boolean; forward;
/// <summary> Check if is running in silent mode (/SILENT or /VERYSILENT) </summary>
function IsSilent: Boolean; forward;
/// <summary> Check if there is already a version of the application installed </summary>
function IsUpgrade: Boolean; forward;
/// <summary> Check if the installation has been executed with /verysilent parameter </summary>
function IsVerySilent: Boolean; forward;
/// <summary> Read a single node text and return a default value if not found </summary>
function ReadXMLNodeText(const AXMLNode: Variant; const AXPath, ADefaultValue: string): string; forward;
/// <summary> Show a message error </summary>
procedure ShowError(const AMessage: string); forward;
/// <summary> Show a formated message error </summary>
procedure ShowErrorFmt(const AMessage: string; const AArgs: TArrayOfString); forward;
/// <summary> Show an information message </summary>
procedure ShowMessage(const AMessage: string); forward;
/// <summary> Try to get the an attribute value of a xml node by the attribute name with case insensitive </summary>
function TryGetXMLNodeAttribute(const AXMLNode: Variant; const AAttributeName: string; out AValue: string): Boolean; forward;
/// <summary> Try to open a filename in notepad. When in very silent mode, it will fail. </summary>
function TryOpenInNotepad(const AFileName: string): Boolean; forward;
/// <summary> Try to open the support url defined by #LibrarySupportURL. When in very silent mode, it will fail. </summary>
function TryOpenSupportURL: Boolean; forward;
/// <summary> Try to open a url. When in very silent mode, it will fail. </summary>
function TryOpenURL(const AURL: string): Boolean; forward;
/// <summary> Try to show a message error if the installation is not in an error state </summary>
function TryShowError(const AMessage: string): Boolean; forward;
/// <summary> Try to show a message error if the installation is not in an error state </summary>
function TryShowErrorEx(const AMessage: string; const ACanReportIssue: Boolean): Boolean; forward;
/// <summary> Try to show a formated message error if the installation is not in an error state </summary>
function TryShowErrorFmt(const AMessage: string; const AArgs: TArrayOfString): Boolean; forward;
/// <summary> Try to show an information message. It will fail when in very silent mode or in error state </summary>
function TryShowMessage(const AMessage: string): Boolean; forward;
/// <summary> Try to uninstall the current version </summary>
function TryUninstallCurrentVersion: Boolean; forward;

// implementation

// uses
  #include ".\String.Utils.inc"

/// <summary> Open the setup logs file </summary>
procedure _OpenSetupLogsFile; forward;
/// <summary> Get the uninstall executable of current version installed in machine. If not found an empty string will be returned. </summary>
function _GetUninstallString: string; forward;
/// <summary> Initialization method of the unit </summary>
procedure _InitializationUnitSetupUtils; forward;
/// <summary> Try to remove the uninstall registry of the app </summary>
function _TryRemoveUninstallRegKey: Boolean; forward;

var
  _FInitializationUnitSetupUtils: Boolean;
  _FIsInErrorState: Boolean;
  _FIsVerySilent: Boolean;

function _GetUninstallString: string;
var
  LUninstallRegKey: string;
begin
  LUninstallRegKey := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#emit SetupSetting("AppId")}_is1');
  Result := '';
  if not RegQueryStringValue(HKEY_LOCAL_MACHINE, LUninstallRegKey, 'UninstallString', Result) then
    RegQueryStringValue(HKEY_CURRENT_USER, LUninstallRegKey, 'UninstallString', Result);
end;

function HasParam(const AParamName: string): Boolean;
var
  I: Integer;
  LCurrentParam: string;
  LSplittedParam: TArrayOfString;
begin
  if AParamName <> '' then
  begin
    for I := 1 to ParamCount do
    begin
      LCurrentParam := ParamStr(I);
      if (Pos('=', LCurrentParam) <> 0) and (Pos('=', AParamName) = 0) then
      begin
        LSplittedParam := SplitString(LCurrentParam, '=');
        if GetArrayLength(LSplittedParam) > 0 then
          LCurrentParam := LSplittedParam[0];
      end;
      if SameText(LCurrentParam, AParamName) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure _InitializationUnitSetupUtils;
var
  I: Integer;
begin
  if _FInitializationUnitSetupUtils then
    Exit;
  _FInitializationUnitSetupUtils := True;
  for I := 1 to ParamCount do
    Log('Setup.Utils._InitializationUnitSetupUtils: Params[' + InttoStr(I) + ']=' + ParamStr(I));
  _FIsVerySilent := HasParam('/VERYSILENT');
end;

function IsAppRunning(const AFileName: string): Boolean;
var
  LSWbemLocator: Variant;
  LWMIService: Variant;
  LWbemObjectSet: Variant;
begin
  try
    LSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    if VarIsNull(LSWbemLocator) then
      Result := False
    else
    begin
      LWMIService := LSWbemLocator.ConnectServer('.', 'root\CIMV2');
      LWbemObjectSet := LWMIService.ExecQuery(Format('SELECT Name FROM Win32_Process WHERE Name="%s"', [AFileName]));
      Result := not VarIsNull(LWbemObjectSet) and (LWbemObjectSet.Count > 0);
    end;
  except
    if SameText(AFileName, 'bds.exe') then
    begin
      Result := (FindWindowByClassName('TAppBuilder') <> 0) and
        (FindWindowByClassName('TPropertyInspector') <> 0) and
        (FindWindowByWindowName('Object Inspector') <> 0);
    end
    else
      Result := False;
  end;
end;

function IsSilent: Boolean;
begin
  Result := IsVerySilent;
  if not Result then
  begin
    if IsUninstaller then
      Result := UninstallSilent
    else
      Result := WizardSilent;
  end;
end;

function IsUpgrade: Boolean;
begin
  Result := _GetUninstallString <> '';
end;

function IsVerySilent: Boolean;
begin
  _InitializationUnitSetupUtils;
  Result := _FIsVerySilent;
end;

procedure _OpenSetupLogsFile;
begin
  if (not IsUninstaller) and (not IsVerySilent) then
    TryOpenInNotepad(ExpandConstant(ExpandConstant('{log}')));
end;

function ReadXMLNodeText(const AXMLNode: Variant; const AXPath, ADefaultValue: string): string;
var
  LXMLNode: Variant;
begin
  LXMLNode := AXMLNode.SelectSingleNode(AXPath);
  if VarIsClear(LXMLNode) then
    Result := ADefaultValue
  else
    Result := LXMLNode.Text;
end;

procedure ShowError(const AMessage: string);
var
  LIsFirstError: Boolean;
begin
  Log('Setup.Utils.ShowError: ' + AMessage);
  LIsFirstError := not _FIsInErrorState;
  _FIsInErrorState := True;
  if not IsVerySilent then
  begin
    #ifdef LibrarySupportURL
    MsgBox(AMessage + CustomMessage('SetupUtilsPleaseReportThiIssue'), mbError, mb_OK);
    TryOpenSupportURL;
    #else
    MsgBox(AMessage, mbError, mb_OK);
    #endif
  end;
  if LIsFirstError then
    _OpenSetupLogsFile;
end;

procedure ShowErrorFmt(const AMessage: string; const AArgs: TArrayOfString);
begin
  ShowError(FmtMessage(AMessage, AArgs));
end;

procedure ShowMessage(const AMessage: string);
begin
  Log('Setup.Utils.ShowMessage: ' + AMessage);
  if not IsVerySilent then
    MsgBox(AMessage, mbInformation, MB_OK);
end;

function TryGetXMLNodeAttribute(const AXMLNode: Variant; const AAttributeName: string; out AValue: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to AXMLNode.Attributes.Length - 1 do
  begin
    if SameText(AXMLNode.Attributes[I].NodeName, AAttributeName) then
    begin
      AValue := AXMLNode.Attributes[I].NodeValue;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TryOpenInNotepad(const AFileName: string): Boolean;
var
  LResultCode: Integer;
begin
  Log('Setup.Utils.TryOpenInNotepad: ' + AFileName);
  Result := (not IsVerySilent) and FileExists(AFileName) and Exec('notepad', '"' + AFileName + '"', '', SW_SHOW, ewNoWait, LResultCode) and (LResultCode = 0);
end;

function TryOpenSupportURL: Boolean;
begin
  Result := TryOpenURL(ExpandConstant('{#LibrarySupportURL}'));
end;

function TryOpenURL(const AURL: string): Boolean;
var
  LResultCode: Integer;
begin
  Log('Setup.Utils.TryOpenURL: ' + AURL);
  Result := (not IsVerySilent) and (AURL <> '') and ShellExec('open', AURL, '', '', SW_SHOW, ewNoWait, LResultCode) and (LResultCode = 0);
end;

function _TryRemoveUninstallRegKey: Boolean;
var
  LUninstallRegKey: string;
begin
  Result := True;
  LUninstallRegKey := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#emit SetupSetting("AppId")}_is1');
  if RegKeyExists(HKEY_LOCAL_MACHINE, LUninstallRegKey) then
    Result := RegDeleteKeyIncludingSubkeys(HKEY_LOCAL_MACHINE, LUninstallRegKey) and Result;
  if RegKeyExists(HKEY_CURRENT_USER, LUninstallRegKey) then
    Result := RegDeleteKeyIncludingSubkeys(HKEY_CURRENT_USER, LUninstallRegKey) and Result;
end;

function TryShowError(const AMessage: string): Boolean;
begin
  Result := TryShowErrorEx(AMessage, True);
end;

function TryShowErrorEx(const AMessage: string; const ACanReportIssue: Boolean): Boolean;
var
  LIsFirstError: Boolean;
begin
  Log('Setup.Utils.TryShowErrorEx: ' + AMessage);
  Result := (not _FIsInErrorState) and (not IsVerySilent);
  LIsFirstError := not _FIsInErrorState;
  _FIsInErrorState := True;
  if Result then
  begin
    #ifdef LibrarySupportURL
    if ACanReportIssue then
    begin
      MsgBox(AMessage + CustomMessage('SetupUtilsPleaseReportThiIssue'), mbError, mb_OK);
      TryOpenSupportURL;
    end
    else
      MsgBox(AMessage, mbError, mb_OK);
    #else
    MsgBox(AMessage, mbError, mb_OK);
    #endif
  end;
  if LIsFirstError and ACanReportIssue then
    _OpenSetupLogsFile;
end;

function TryShowErrorFmt(const AMessage: string; const AArgs: TArrayOfString): Boolean;
begin
  case GetArrayLength(AArgs) of
    0: Result := TryShowError(AMessage);
    1: Result := TryShowError(FmtMessage(AMessage, [AArgs[0]]));
    2: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1]]));
    3: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1], AArgs[2]]));
    4: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1], AArgs[2], AArgs[3]]));
    5: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1], AArgs[2], AArgs[3], AArgs[4]]));
    6: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1], AArgs[2], AArgs[3], AArgs[4], AArgs[5]]));
    7: Result := TryShowError(FmtMessage(AMessage, [AArgs[0], AArgs[1], AArgs[2], AArgs[3], AArgs[4], AArgs[5], AArgs[6]]));
  else
  begin
    Log('Setup.Utils.TryShowErrorFmt: Cannot possible to format the message "' + AMessage + '"');
    Result := TryShowError(AMessage);
  end;
  end;
end;

function TryShowMessage(const AMessage: string): Boolean;
begin
  Log('Setup.Utils.TryShowMessage: ' + AMessage);
  Result := (not _FIsInErrorState) and (not IsVerySilent);
  if Result then
    MsgBox(AMessage, mbInformation, MB_OK);
end;

function TryUninstallCurrentVersion: Boolean;
var
  LUninstallString: string;
  LExtraParam: string;
  LResultCode: Integer;
begin
  LExtraParam := '';
  if IsVerySilent then
    LExtraParam := ' /VERYSILENT';
  LUninstallString := _GetUninstallString;
  Log('Setup.Utils.TryUninstallCurrentVersion: ' + LUninstallString);
  if LUninstallString = '' then
  begin
    Result := True;
    Exit;
  end;
  LUninstallString := RemoveQuotes(LUninstallString);
  if FileExists(LUninstallString) then
    Result := (Exec(LUninstallString, '/SILENT /NORESTART /SUPPRESSMSGBOXES' + LExtraParam, '', SW_HIDE, ewWaitUntilTerminated, LResultCode) and (LResultCode = 0))
  else
    Result := _TryRemoveUninstallRegKey;
end;

// end.
#endif
