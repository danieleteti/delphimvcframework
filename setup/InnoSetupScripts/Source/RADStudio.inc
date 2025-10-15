#ifndef UnitRADStudio
#define UnitRADStudio

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
// unit RADStudio;

// interface

// uses
  #include ".\RADStudio.Project.inc"

type
  TRADStudioVersion = record
    Name: string;
    RegVersion: string;
    SupportedPlatforms: TProjectPlatforms;
    SupportedPlatformsDelphi: TProjectPlatforms;
    MaxDprojVersion: string;
    MinDprojVersion: string;
    PackageVersion: string;
    OldLibSuffix: string;
    HasGetItCmd: Boolean;
    GetItCmdListInstalledParams: string;
    GetItCmdUninstallParams: string;
    DotNetVersion: TDotNetVersion;
    DotNetServicePack: Cardinal;
  end;

  TRADStudioInstallationStatus = (riNotFound, riNeedOpenFirst, riToolchainNotSupported, riNormal);

  TRADStudioInfo = record
    AppFileName: string;
    BdsCommonDir: string;
    BdsInclude: string;
    BuildConfig: TProjectConfig;
    RootDir: string;
    Status: TRADStudioInstallationStatus;
    Version: TRADStudioVersion;
  end;

  TRADStudioInfos = array of TRADStudioInfo;

/// <summary> Checks if RAD Studio supports the platform </summary>
function CheckIfRADStudioSupportsPlatform(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): Boolean; forward;
/// <summary> Compare version of RAD Studio </summary>
function CompareRADStudioVersion(const ARADStudioVersion: TRADStudioVersion; const ARegVersion: string): Integer; forward;
/// <summary> Compare two versions of RAD Studio </summary>
function CompareRADStudioVersions(const AVersion1, AVersion2: TRADStudioVersion): Integer; forward;
/// <summary> Get common directory of RAD Studio BPLs </summary>
function GetRADStudioCommonBplDir(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): string; forward;
/// <summary> Get common directory of RAD Studio DCPs </summary>
function GetRADStudioCommonDcpDir(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): string; forward;
/// <summary> Get the RAD Studio info </summary>
function GetRADStudioInfo(const ARADStudioVersion: TRADStudioVersion): TRADStudioInfo; forward;
/// <summary> Get RAD Studio infos from the RAD Studio versions supported by each packages of each group project </summary>
function GetRADStudioInfosSupportedByGroupProjects(const AGroupProjects: TRADStudioGroupProjects): TRADStudioInfos; forward;
/// <summary> Get the RAD Studio path from the registry </summary>
function GetRADStudioRegKey(const ARADStudioVersion: TRADStudioVersion): string; forward;
/// <summary> Check if there is any RAD Studio instance running in machine </summary>
function HaveRADStudioInstanceRunning: Boolean; forward;
/// <summary> Try to add an environment variable exclusive of RAD Studio application </summary>
function TryAddRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName, AValue: string): Boolean; forward;
/// <summary> Try to add a path to the library browsing path of the RAD Studio </summary>
function TryAddRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean; forward;
/// <summary> Try to add a path to the library debug dcu path of the RAD Studio </summary>
function TryAddRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean; forward;
/// <summary> Try to add a path to the library search path of the RAD Studio </summary>
function TryAddRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean; forward;
/// <summary> Try to add a path to the PATH environment variable exclusive of RAD Studio application </summary>
function TryAddRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean; forward;
/// <summary> Try to get the newest version of RAD Studio detected in projects of a group projects array </summary>
function TryGetMaxRADStudioVersionInGroupProjects(const AGroupProjects: TRADStudioGroupProjects; out ARADStudioVersion: TRADStudioVersion): Boolean; forward;
/// <summary> Try get default BPL output Path + filename of a RAD Studio project </summary>
function TryGetRADStudioProjectBplOutputFileName(const AProject: TRADStudioProject; const ADllSuffix: string; out AFileName: string): Boolean; forward;
/// <summary> Try get default BPL output filename of a RAD Studio project </summary>
function TryGetRADStudioBplFileName(const AProject: TRADStudioProject; const ABplSuffix: string; out AFileName: string): Boolean; forward;
/// <summary> Try to get the "rsvars.bat" filename, used to compile projects with MSBuild </summary>
function TryGetRADStudioRSVarsBatchFileName(const ARADStudioInfo: TRADStudioInfo; out ARsvarsFileName: string): Boolean; forward;
/// <summary> Try to get the RAD Studio version by product version in registry </summary>
function TryGetRADStudioVersionByRegVersion(const ARegVersion: string; out ARADStudioVersion: TRADStudioVersion): Boolean; forward;
/// <summary> Try to get the RAD Studio version of a project </summary>
function TryGetRADStudioVersionOfProject(const AProject: TRADStudioProject; out ARADStudioVersion: TRADStudioVersion): Boolean; forward;
/// <summary> Calculate Registry Node Name for KnownPackages</summary>
function GetKnownPackagesNodeName(const APlatform: TProjectPlatform): string; forward;
/// <summary> Try to register a Bpl in RAD Studio </summary>
function TryRegisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ABplFileName, ADescription: string): Boolean; forward;
/// <summary> Try to remove a library instaled using the GetIt </summary>
function TryRemoveFromGetIt(const AInfos: TRADStudioInfos; const ALibraryName: string): Boolean; forward;
/// <summary> Try to remove an environment variable exclusive of RAD Studio application </summary>
function TryRemoveRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName: string): Boolean; forward;
/// <summary> Try to remove a path from the library browsing path of the RAD Studio </summary>
function TryRemoveRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path from the library debug dcu path of the RAD Studio </summary>
function TryRemoveRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path from the library search path of the RAD Studio </summary>
function TryRemoveRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path to the PATH environment variable exclusive of RAD Studio application </summary>
function TryRemoveRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean; forward;
/// <summary> Try to unregister a Bpl in RAD Studio </summary>
function TryUnregisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ABplFileName: string): Boolean; forward;

// implementation

// uses
  #include ".\Setup.Utils.inc"
  #include ".\String.Utils.inc"

type
  _TRADStudioVersions = array of TRADStudioVersion;

/// <summary> Filter allowed version defined by the parameter /RADStudioVersions=all for example </summary>
function _FilterAllowedRADStudioVersions(const ARADStudioVersions: _TRADStudioVersions): _TRADStudioVersions; forward;
/// <summary> Try to find and add new versions (such as beta versions) </summary>
procedure _FindAndAddNewRADStudioVersions(var ARADStudioVersions: _TRADStudioVersions); forward;
/// <summary> Get RAD Studio installation status name </summary>
function _GetRADStudioInstallationStatusName(const AStatus: TRADStudioInstallationStatus): string; forward;
/// <summary> Get the RAD Studio versions that the installer is able to recognize </summary>
function _GetRADStudioVersions: _TRADStudioVersions; forward;
/// <summary> Initialization method of the unit </summary>
procedure _InitializationUnitRADStudio; forward;
/// <summary> Sort RAD Studio infos by version </summary>
function _SortRADStudioInfos(AInfos: TRADStudioInfos): TRADStudioInfos; forward;
/// <summary> Sort RAD Studio versions </summary>
function _SortRADStudioVersions(AVersions: _TRADStudioVersions): _TRADStudioVersions; forward;
/// <summary> Try to add a path to the Library registry (in a specific registry name) of the RAD Studio </summary>
function _TryAddRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string;
  const APos: Integer): Boolean; forward;
/// <summary> Try to get the RADStudio environment variable </summary>
function _TryGetRADStudioEnvVar(const ARADStudioVersion: TRADStudioVersion; const AName: string; out AValue: string): Boolean; forward;
/// <summary> Try to get the RADStudio installations info </summary>
function _TryGetRADStudioInstallation(const ARADStudioVersion: TRADStudioVersion; const ARootKey: Integer; out AAppFileName, ARootDir: string): Boolean; forward;
/// <summary> Try to get the RADStudio package version by it's product version </summary>
function _TryGetRADStudioPackageVersionByProductVersion(AProductVersion: string; out APackageVersion: string): Boolean; forward;
/// <summary> Try to remove a bpl from the "disabled packages" registry of the RAD Studio </summary>
function _TryRemoveRADStudioBplFromDisabledPackages(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean; forward;
/// <summary> Try to remove a path from the Library registry (in a specific registry name) of the RAD Studio </summary>
function _TryRemoveRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean; forward;

const
  _RADStudioCommonSubKeyReg = 'Software\Embarcadero\BDS';

var
  _FRADStudioVersions: _TRADStudioVersions;

function CheckIfRADStudioSupportsPlatform(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): Boolean;
begin
  Result := (APlatform in ARADStudioInfo.Version.SupportedPlatforms) and
    RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioInfo.Version) + '\Library\' + GetProjectPlatformLibraryName(APlatform));
end;

function CompareRADStudioVersion(const ARADStudioVersion: TRADStudioVersion; const ARegVersion: string): Integer;
begin
  Result := CompareVersions(ARADStudioVersion.RegVersion, ARegVersion);
end;

function CompareRADStudioVersions(const AVersion1, AVersion2: TRADStudioVersion): Integer;
begin
  Result := CompareVersions(AVersion1.RegVersion, AVersion2.RegVersion);
end;

function _FilterAllowedRADStudioVersions(const ARADStudioVersions: _TRADStudioVersions): _TRADStudioVersions;
var
  LAllowedVersionsText: TArrayOfString;
  LAllowedVersions: array of Int64;
  LVersion: Int64;
  I, J: Integer;
begin
  LAllowedVersionsText := SplitString(ExpandConstant('{param:RADStudioVersions|all}'), ',');
  LAllowedVersions := [];
  for I := 0 to GetArrayLength(LAllowedVersionsText) - 1 do
  begin
    if SameText(LAllowedVersionsText[I], 'all') then
    begin
      Result := ARADStudioVersions;
      Exit;
    end;
    if StrToVersion(LAllowedVersionsText[I], LVersion) then
    begin
      SetArrayLength(LAllowedVersions, GetArrayLength(LAllowedVersions) + 1);
      LAllowedVersions[GetArrayLength(LAllowedVersions) - 1] := LVersion;
    end;
  end;

  Result := [];
  for I := 0 to GetArrayLength(ARADStudioVersions) - 1 do
  begin
    if StrToVersion(ARADStudioVersions[I].RegVersion, LVersion) then
    begin
      for J := 0 to GetArrayLength(LAllowedVersions) - 1 do
      begin
        if LVersion = LAllowedVersions[J] then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := ARADStudioVersions[I];
          Break;
        end;
      end;
    end;
  end;
end;

procedure _FindAndAddNewRADStudioVersions(var ARADStudioVersions: _TRADStudioVersions);
var
  LChanged: Boolean;
  LMaxKnownRADStudioVersion: TRADStudioVersion;
  LMaxKnownVersion: Int64;
  LNames: TArrayOfString;
  LProductVersion: string;
  LRADStudioInfo: TRADStudioInfo;
  LRADStudioVersion: TRADStudioVersion;
  LVersion: Int64;
  I: Integer;
begin
  if GetArrayLength(ARADStudioVersions) = 0 then
    Exit;
  LMaxKnownRADStudioVersion := ARADStudioVersions[GetArrayLength(ARADStudioVersions) - 1];
  if not StrToVersion(LMaxKnownRADStudioVersion.RegVersion, LMaxKnownVersion) then
    Exit;

  if RegGetSubkeyNames(HKEY_CURRENT_USER, _RADStudioCommonSubKeyReg, LNames) then
  begin
    LChanged := False;
    for I := 0 to GetArrayLength(LNames) - 1 do
    begin
      if StrToVersion(LNames[I], LVersion) and (LVersion > LMaxKnownVersion) then
      begin
        LRADStudioVersion := LMaxKnownRADStudioVersion;
        LRADStudioVersion.RegVersion := LNames[I];
        if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(LRADStudioVersion), 'ProductVersion', LProductVersion)) or
          not _TryGetRADStudioPackageVersionByProductVersion(LProductVersion, LRADStudioVersion.PackageVersion) then
        begin
          Continue;
        end;
        LRADStudioVersion.MaxDprojVersion := Format('%d.0', [10000 + StrToIntDef(LRADStudioVersion.PackageVersion, 0)]);
        LRADStudioVersion.MinDprojVersion := LRADStudioVersion.MaxDprojVersion;
        if not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(LRADStudioVersion) + '\Personalities\', '', LRADStudioVersion.Name) then
          LRADStudioVersion.Name := CustomMessage('RADStudioUnknownVersionName');
        LRADStudioVersion.Name := LRADStudioVersion.Name + ' (BDS ' + LRADStudioVersion.RegVersion + ')';

        LRADStudioInfo := GetRADStudioInfo(LRADStudioVersion);
        if LRADStudioInfo.Status in [riToolchainNotSupported, riNormal] then
        begin
          SetArrayLength(ARADStudioVersions, GetArrayLength(ARADStudioVersions) + 1);
          ARADStudioVersions[GetArrayLength(ARADStudioVersions) - 1] := LRADStudioVersion;
          LChanged := True;
        end;
      end;
    end;
    if LChanged then
      ARADStudioVersions := _SortRADStudioVersions(ARADStudioVersions);
  end;
end;

function GetRADStudioCommonBplDir(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): string;
begin
  Result := ARADStudioInfo.BdsCommonDir;
  if Result <> '' then
  begin
    Result := CombinePath(Result, 'Bpl');
    if APlatform <> pfWin32 then
      Result := CombinePath(Result, GetProjectPlatformName(APlatform));
  end;
end;

function GetRADStudioCommonDcpDir(const ARADStudioInfo: TRADStudioInfo; const APlatform: TProjectPlatform): string;
begin
  Result := ARADStudioInfo.BdsCommonDir;
  if Result <> '' then
  begin
    Result := CombinePath(Result, 'Dcp');
    if APlatform <> pfWin32 then
      Result := CombinePath(Result, GetProjectPlatformName(APlatform));
  end;
end;

function GetRADStudioInfo(const ARADStudioVersion: TRADStudioVersion): TRADStudioInfo;
var
  LBdsLib: string;
  LResultCode: Integer;
begin
  Result.Version := ARADStudioVersion;
  Result.BdsCommonDir := '';
  Result.BdsInclude := '';
  Result.BuildConfig := pcRelease;
  if _TryGetRADStudioInstallation(ARADStudioVersion, HKEY_CURRENT_USER, Result.AppFileName, Result.RootDir) then
  begin
    if (not Exec(ExpandConstant('{cmd}'), Format('/C "%s" --test > nul 2>&1', [ExtractFilePath(Result.AppFileName) + 'DCC32.EXE']), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or (LResultCode = 0) then
      Result.Status := riToolchainNotSupported
    else
    begin
      Result.Status := riNormal;
      _TryGetRADStudioEnvVar(ARADStudioVersion, 'BDSCOMMONDIR', Result.BdsCommonDir);
      if not DirExists(Result.BdsCommonDir) then
        Result.BdsCommonDir := '';
      _TryGetRADStudioEnvVar(ARADStudioVersion, 'BDSINCLUDE', Result.BdsInclude);
      if not DirExists(Result.BdsInclude) then
        Result.BdsInclude := '';
      // Some beta / internal versions does not have release libs
      if _TryGetRADStudioEnvVar(ARADStudioVersion, 'BDSLIB', LBdsLib) then
      begin
        if FileExists(CombinePath(LBdsLib, 'win32\debug\rtl.dcp')) and
          not FileExists(CombinePath(LBdsLib, 'win32\release\rtl.dcp')) then
        begin
          Result.BuildConfig := pcDebug;
        end;
      end;
    end;
  end
  else if _TryGetRADStudioInstallation(ARADStudioVersion, HKEY_LOCAL_MACHINE, Result.AppFileName, Result.RootDir) then
    Result.Status := riNeedOpenFirst
  else
    Result.Status := riNotFound;
end;

function GetRADStudioInfosSupportedByGroupProjects(const AGroupProjects: TRADStudioGroupProjects): TRADStudioInfos;
var
  LVersion: TRADStudioVersion;
  LMaxVersionInGroupProjects: TRADStudioVersion;
  LFound: Boolean;
  I, J, K: Integer;
  Z: Integer;
  LProject: TRADStudioProject;
begin
  Log('RADStudio.GetRADStudioInfosSupportedByGroupProjects: Searching all RAD Studio versions that is installed in machine and is compatible with library...');
  Result := [];
  // Search RAD Studio versions in packages
  Z := GetArrayLength(AGroupProjects);
  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
  begin
    for J := 0 to GetArrayLength(AGroupProjects[I].Items) - 1 do
    begin
      LProject := AGroupProjects[I].Items[J].Project;
      if TryGetRADStudioVersionOfProject(LProject, LVersion) then
      begin
        // Avoiding duplications before insertion
        LFound := False;
        for K := 0 to GetArrayLength(Result) - 1 do
        begin
          if CompareRADStudioVersions(Result[K].Version, LVersion) = 0 then
          begin
            LFound := True;
            Break;
          end;
        end;
        if not LFound then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := GetRADStudioInfo(LVersion);

          if (GetArrayLength(Result) = 1) or (CompareRADStudioVersions(LMaxVersionInGroupProjects, LVersion) < 0) then
            LMaxVersionInGroupProjects := LVersion;
        end;
      end;
    end;
  end;

  // Adding newer versions (which theoretically are not officially supported by the packages) to be able to try to install them
(*  
  if GetArrayLength(Result) > 0 then
  begin
    for I := 0 to GetArrayLength(_FRADStudioVersions) - 1 do
      if CompareRADStudioVersions(LMaxVersionInGroupProjects, _FRADStudioVersions[I]) < 0 then
      begin
        SetArrayLength(Result, GetArrayLength(Result) + 1);
        Result[GetArrayLength(Result) - 1] := GetRADStudioInfo(_FRADStudioVersions[I]);
      end;
  end;
*)
  // Sort results
  Result := _SortRADStudioInfos(Result);

  for I := 0 to GetArrayLength(Result) - 1 do
  begin
    Log(Format('RADStudio.GetRADStudioInfosSupportedByGroupProjects: Found %s:', [Result[I].Version.Name]));
    Log(Format('RADStudio.GetRADStudioInfosSupportedByGroupProjects:   > AppFileName: "%s"', [Result[I].AppFileName]));
    Log(Format('RADStudio.GetRADStudioInfosSupportedByGroupProjects:   > RootDir: "%s"', [Result[I].RootDir]));
    Log(Format('RADStudio.GetRADStudioInfosSupportedByGroupProjects:   > Status: "%s"', [_GetRADStudioInstallationStatusName(Result[I].Status)]));
    Log(Format('RADStudio.GetRADStudioInfosSupportedByGroupProjects:   > RegKey: "%s"', [GetRADStudioRegKey(Result[I].Version)]));
  end;
end;

function _GetRADStudioInstallationStatusName(const AStatus: TRADStudioInstallationStatus): string;
begin
  case AStatus of
    riNotFound             : Result := 'Not found';
    riNeedOpenFirst        : Result := 'Need open first';
    riToolchainNotSupported: Result := 'Toolchain not supported';
    riNormal               : Result := 'Normal';
  else
    Result := '';
  end;
end;

function GetRADStudioRegKey(const ARADStudioVersion: TRADStudioVersion): string;
begin
  Result := AddBackslash(_RADStudioCommonSubKeyReg) + ARADStudioVersion.RegVersion;
end;

function _GetRADStudioVersions: _TRADStudioVersions;
begin
  _InitializationUnitRADStudio;
  Result := _FRADStudioVersions;
end;

function HaveRADStudioInstanceRunning: Boolean;
begin
  Result := IsAppRunning('bds.exe');
end;

procedure _InitializationUnitRADStudio;
var
  LRADStudioVersion: TRADStudioVersion;
begin
  if GetArrayLength(_FRADStudioVersions) <> 0 then
    Exit;
  // Note: Keep the _FRADStudioVersions array in order, from the oldest version to the newest

  // RAD Studio XE3
  LRADStudioVersion.Name := 'RAD Studio XE3';
  LRADStudioVersion.RegVersion := '10.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '14.4';
  LRADStudioVersion.MinDprojVersion := '14.0';
  LRADStudioVersion.PackageVersion := '170';
  LRADStudioVersion.OldLibSuffix := 'DXE3';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE6
  LRADStudioVersion.Name := 'RAD Studio XE6';
  LRADStudioVersion.RegVersion := '14.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '15.4';
  LRADStudioVersion.MinDprojVersion := '15.1';
  LRADStudioVersion.PackageVersion := '200';
  LRADStudioVersion.OldLibSuffix := 'DXE6';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE7
  LRADStudioVersion.Name := 'RAD Studio XE7';
  LRADStudioVersion.RegVersion := '15.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSSimulator, pfOSX32];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '16.1';
  LRADStudioVersion.MinDprojVersion := '16.0';
  LRADStudioVersion.PackageVersion := '210';
  LRADStudioVersion.OldLibSuffix := 'DXE7';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE8
  LRADStudioVersion.Name := 'RAD Studio XE8';
  LRADStudioVersion.RegVersion := '16.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '17.2';
  LRADStudioVersion.MinDprojVersion := '17.1';
  LRADStudioVersion.PackageVersion := '220';
  LRADStudioVersion.OldLibSuffix := 'DXE8';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.0 Seattle
  LRADStudioVersion.Name := 'RAD Studio 10 Seattle';
  LRADStudioVersion.RegVersion := '17.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '18.1'; // Conflicted
  LRADStudioVersion.MinDprojVersion := '18.0';
  LRADStudioVersion.PackageVersion := '230';
  LRADStudioVersion.OldLibSuffix := 'D10_0';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.1 Berlin
  LRADStudioVersion.Name := 'RAD Studio 10.1 Berlin';
  LRADStudioVersion.RegVersion := '18.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '18.2'; // Conflicted
  LRADStudioVersion.MinDprojVersion := '18.1'; // Conflicted
  LRADStudioVersion.PackageVersion := '240';
  LRADStudioVersion.OldLibSuffix := 'D10_1';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.2 Tokyo
  LRADStudioVersion.Name := 'RAD Studio 10.2 Tokyo';
  LRADStudioVersion.RegVersion := '19.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '18.4';
  LRADStudioVersion.MinDprojVersion := '18.2'; // Conflicted
  LRADStudioVersion.PackageVersion := '250';
  LRADStudioVersion.OldLibSuffix := 'D10_2';
  LRADStudioVersion.HasGetItCmd := False; // It has but is obsolete
  LRADStudioVersion.GetItCmdListInstalledParams := '-listavailable:"%s" -filter:Installed';
  LRADStudioVersion.GetItCmdUninstallParams := '-u"%s"';
  LRADStudioVersion.DotNetVersion := net35;
  LRADStudioVersion.DotNetServicePack := 1;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.3 Rio
  LRADStudioVersion.Name := 'RAD Studio 10.3 Rio';
  LRADStudioVersion.RegVersion := '20.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32, pfOSX64, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '18.8';
  LRADStudioVersion.MinDprojVersion := '18.5';
  LRADStudioVersion.PackageVersion := '260';
  LRADStudioVersion.OldLibSuffix := 'D10_3';
  LRADStudioVersion.HasGetItCmd := False; // It has but is obsolete
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -dnsef';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s" -dnsef';
  LRADStudioVersion.DotNetVersion := net45;
  LRADStudioVersion.DotNetServicePack := 0;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.4 Sydney
  LRADStudioVersion.Name := 'RAD Studio 10.4 Sydney';
  LRADStudioVersion.RegVersion := '21.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimulator, pfOSX64, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '19.2';
  LRADStudioVersion.MinDprojVersion := '19.0';
  LRADStudioVersion.PackageVersion := '270';
  LRADStudioVersion.OldLibSuffix := 'D10_4';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -dnsef';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s" -dnsef';
  LRADStudioVersion.DotNetVersion := net45;
  LRADStudioVersion.DotNetServicePack := 0;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 11 Alexandria
  LRADStudioVersion.Name := 'RAD Studio 11 Alexandria';
  LRADStudioVersion.RegVersion := '22.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimARM64, pfiOSSimulator, pfOSX64, pfOSXARM64, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '19.5';
  LRADStudioVersion.MinDprojVersion := '19.3';
  LRADStudioVersion.PackageVersion := '280';
  LRADStudioVersion.OldLibSuffix := 'D11';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -dnsef';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s" -dnsef';
  LRADStudioVersion.DotNetVersion := net45;
  LRADStudioVersion.DotNetServicePack := 0;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 12 Athens
  LRADStudioVersion.Name := 'RAD Studio 12 Athens';
  LRADStudioVersion.RegVersion := '23.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfWin64x, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimARM64, pfOSX64, pfOSXARM64, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '20.2'; // 20.3 Conflicted RAD Studio 13
  LRADStudioVersion.MinDprojVersion := '20.1';
  LRADStudioVersion.PackageVersion := '290';
  LRADStudioVersion.OldLibSuffix := 'D12';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -dnsef';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s" -dnsef';
  LRADStudioVersion.DotNetVersion := net45;
  LRADStudioVersion.DotNetServicePack := 0;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 13 Florence
  LRADStudioVersion.Name := 'RAD Studio 13 Florence';
  LRADStudioVersion.RegVersion := '37.0';
  LRADStudioVersion.SupportedPlatformsDelphi := [pfWin32, pfWin64, pfWin64x, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimARM64, pfOSX64, pfOSXARM64, pfLinux64];
  LRADStudioVersion.SupportedPlatforms := LRADStudioVersion.SupportedPlatformsDelphi;
  LRADStudioVersion.MaxDprojVersion := '20.3'; // Use it for RAD Studio 13
  LRADStudioVersion.MinDprojVersion := '20.3';
  LRADStudioVersion.PackageVersion := '370';
  LRADStudioVersion.OldLibSuffix := 'D13';  
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -dnsef';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s" -dnsef';
  LRADStudioVersion.DotNetVersion := net45;
  LRADStudioVersion.DotNetServicePack := 0;
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  _FindAndAddNewRADStudioVersions(_FRADStudioVersions);
  _FRADStudioVersions := _FilterAllowedRADStudioVersions(_FRADStudioVersions);
end;

function _SortRADStudioInfos(AInfos: TRADStudioInfos): TRADStudioInfos;
var
  I, J: Integer;
  LMinIndex: Integer;
  LVersion: TRADStudioVersion;
  LResult: TRADStudioInfos;
begin
  SetArrayLength(LResult, GetArrayLength(AInfos));
  for I := 0 to GetArrayLength(AInfos) - 1 do
  begin
    LMinIndex := I;
    for J := 0 to GetArrayLength(AInfos) - 1 do
    begin
      if (AInfos[J].Version.RegVersion <> '') and ((AInfos[LMinIndex].Version.RegVersion = '') or
        (CompareRADStudioVersions(AInfos[J].Version, AInfos[LMinIndex].Version) < 0)) then
      begin
        LMinIndex := J;
      end;
    end;
    LResult[I] := AInfos[LMinIndex];
    LVersion := AInfos[LMinIndex].Version;
    LVersion.RegVersion := '';
    AInfos[LMinIndex].Version := LVersion;
  end;
  Result := LResult;
end;

function _SortRADStudioVersions(AVersions: _TRADStudioVersions): _TRADStudioVersions;
var
  I, J: Integer;
  LMinIndex: Integer;
  LVersion: TRADStudioVersion;
  LResult: _TRADStudioVersions;
begin
  SetArrayLength(LResult, GetArrayLength(AVersions));
  for I := 0 to GetArrayLength(AVersions) - 1 do
  begin
    LMinIndex := I;
    for J := 0 to GetArrayLength(AVersions) - 1 do
    begin
      if (AVersions[J].RegVersion <> '') and ((AVersions[LMinIndex].RegVersion = '') or
        (CompareRADStudioVersions(AVersions[J], AVersions[LMinIndex]) < 0)) then
      begin
        LMinIndex := J;
      end;
    end;
    LResult[I] := AVersions[LMinIndex];
    LVersion := AVersions[LMinIndex];
    LVersion.RegVersion := '';
    AVersions[LMinIndex] := LVersion;
  end;
  Result := LResult;
end;

function TryAddRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName, AValue: string): Boolean;
begin
  Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName, AValue);
  if not Result then
    Log(Format('RADStudio.TryAddRADStudioEnvVariable: Failed to add an environment variable to the RAD Studio registry (Name: "%s", Value: "%s")', [AName, AValue]));
end;

function TryAddRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Browsing Path', APath, APos);
end;

function TryAddRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Debug DCU Path', APath, APos);
end;

function _TryAddRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string;
  const APos: Integer): Boolean;
var
  LNewValue: string;
  LPathsToInsert: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I, LArraySize: Integer;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform));
  if Result then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue)) or (LNewValue = '') then
      LNewValue := APath
    else
    begin
      LPathsToInsert := SplitString(APath, ';');
      LCurrentPaths := SplitString(LNewValue, ';');
      for I := GetArrayLength(LPathsToInsert) - 1 downto 0 do
      begin
        LNewValue := LPathsToInsert[I];
        LCurrentPaths := RemoveString(LCurrentPaths, LNewValue, False);
        LCurrentPaths := RemoveString(LCurrentPaths, LNewValue + '\', False);
        //If Position is not specified or is grather than last position, add to the last position
        LArraySize := GetArrayLength(LCurrentPaths);
        if (APos < 0) or (APos > LArraySize) then
          APos := LArraySize;
        LCurrentPaths := InsertString(APos, LCurrentPaths, LNewValue, False);
      end;
      LNewValue := JoinStrings(LCurrentPaths, ';', False);
    end;
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue);
  end;
  if not Result then
    Log(Format('RADStudio._TryAddRADStudioLibraryPath: Failed to add a library path to the RAD Studio registry (RegName: "%s", Path: "%s")', [ARegName, APath]));
end;

function TryAddRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string;
  const APos: Integer): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Search Path', APath, APos);
end;

function TryAddRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean;
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion));
  if Result then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LPaths := InsertStringAtBeginning(LPaths, APath);
    LValue := JoinStrings(LPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue);
  end;
  if not Result then
    Log(Format('RADStudio.TryAddRADStudioPathEnvVariable: Failed to add a path environment variable to the RAD Studio registry (Path: "%s")', [APath]));
end;

function TryGetMaxRADStudioVersionInGroupProjects(const AGroupProjects: TRADStudioGroupProjects; out ARADStudioVersion: TRADStudioVersion): Boolean;
var
  I: Integer;
  J: Integer;
  LVersion: TRADStudioVersion;
begin
  Result := False;
  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
  begin
    for J := 0 to GetArrayLength(AGroupProjects[I].Items) - 1 do
    begin
      if TryGetRADStudioVersionOfProject(AGroupProjects[I].Items[J].Project, LVersion) and
        (not Result) or (CompareRADStudioVersions(LVersion, ARADStudioVersion) > 0) then
      begin
        ARADStudioVersion := LVersion;
        Result := True;
      end;
    end;
  end;
  if not Result then
    Log('RADStudio.TryGetMaxRADStudioVersionInGroupProjects: Failed to get the max RAD Studio version in group projects');
end;

function _TryGetRADStudioEnvVar(const ARADStudioVersion: TRADStudioVersion; const AName: string; out AValue: string): Boolean;
var
  LXMLDocument: Variant;
  LFileName: string;
begin
  LFileName := ExpandConstant(Format('{userappdata}\Embarcadero\BDS\%s\environment.proj', [ARADStudioVersion.RegVersion]));
  Result := FileExists(LFileName);
  AValue := '';
  if Result then
  begin
    LXMLDocument := CreateOleObject('Msxml2.DOMDocument');
    try
      LXMLDocument.Async := False;
      LXMLDocument.ResolveExternals := False;
      LXMLDocument.Load(LFileName);
      if LXMLDocument.ParseError.ErrorCode <> 0 then
      begin
        Log(Format('RADStudio._TryGetRADStudioEnvVar: Error parsing the file "%s" (code %d, line %d, position %d, message "%s")', [LFileName, LXMLDocument.ParseError.ErrorCode, LXMLDocument.ParseError.Line, LXMLDocument.ParseError.LinePos, LXMLDocument.ParseError.Reason]));
        Result := False;
        Exit;
      end;

      AValue := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup/' + AName, '<{+error+}>');
      Result := AValue <> '<{+error+}>';
      if not Result then
        AValue := '';
    except
      Log(Format('RADStudio._TryGetRADStudioEnvVar: Unexpected error parsing the file "%s" (message "%s")', [LFileName, GetExceptionMessage]));
      Result := False;
    end;
  end;
end;

function _TryGetRADStudioInstallation(const ARADStudioVersion: TRADStudioVersion; const ARootKey: Integer; out AAppFileName, ARootDir: string): Boolean;
begin
  try
    if not RegQueryStringValue(ARootKey, GetRADStudioRegKey(ARADStudioVersion), 'RootDir', ARootDir) then
    begin
      Result := False;
      Exit;
    end;
    if not RegQueryStringValue(ARootKey, GetRADStudioRegKey(ARADStudioVersion), 'App', AAppFileName) then
      AAppFileName := AddBackslash(AddBackslash(ARootDir) + 'bin') + 'bds.exe';
    if not FileExists(AAppFileName) then
    begin
      Result := False;
      Exit;
    end;
    ARootDir := RemoveBackslashUnlessRoot(ARootDir);
    Result := True;
  finally
    if not Result then
    begin
      AAppFileName := '';
      ARootDir := '';
    end;
  end;
end;

function _TryGetRADStudioPackageVersionByProductVersion(AProductVersion: string; out APackageVersion: string): Boolean;
var
  LProductVersion: Int64;
  LMajor, LMinor, LRevision, LBuild: Word;
begin
  Result := StrToVersion(AProductVersion, LProductVersion) and (LProductVersion > 0);
  if Result then
  begin
    UnpackVersionComponents(LProductVersion, LMajor, LMinor, LRevision, LBuild);
    APackageVersion := Format('%d%d', [LMajor, LMinor]);
  end;
end;

function TryGetRADStudioProjectBplOutputFileName(const AProject: TRADStudioProject; const ADllSuffix: string; out AFileName: string): Boolean;
begin
  Result := TryGetRADStudioBplFileName(AProject, ADllSuffix, AFileName);
  if Result then
    AFileName := AddBackslash(AProject.BplOutputPath) + AFileName;
end;

function TryGetRADStudioBplFileName(const AProject: TRADStudioProject; const ABplSuffix: string; out AFileName: string): Boolean;
var
  LBplSuffix: string;
  LVersion: TRADStudioVersion;
begin
  LBplSuffix := ABplSuffix;
  if SameText(LBplSuffix, '$(Auto)') then
  begin
    Result := TryGetRADStudioVersionOfProject(AProject, LVersion);
    if not Result then
    begin
      AFileName := '';
      Log(Format('RADStudio.TryGetRADStudioProjectBplOutputFileName: Failed to get the RAD Studio project Bpl''s filename, of project "%s".', [AProject.FileName]));
      Exit;
    end;
    LBplSuffix := LVersion.PackageVersion;
  end;
  AFileName := ExtractFileName(ChangeFileExt(AProject.FileName, LBplSuffix + '.bpl'));
  Result := True;
end;

function TryGetRADStudioRSVarsBatchFileName(const ARADStudioInfo: TRADStudioInfo; out ARsvarsFileName: string): Boolean;
begin
  if ARADStudioInfo.RootDir = '' then
    Result := False
  else
  begin
    ARsvarsFileName := CombinePath(ExtractFilePath(ARADStudioInfo.AppFileName), 'rsvars.bat');
    Result := FileExists(ARsvarsFileName);
  end;
  if not Result then
  begin
    ARsvarsFileName := '';
    Log(Format('RADStudio.TryGetRADStudioRSVarsBatchFileName: Failed to get the "rsvars.bat" filename of the %s.', [ARADStudioInfo.Version.Name]));
  end;
end;

function TryGetRADStudioVersionByRegVersion(const ARegVersion: string; out ARADStudioVersion: TRADStudioVersion): Boolean;
var
  LVersion: Int64;
  LRADVersion: Int64;
  LRADStudioVersions: _TRADStudioVersions;
  I: Integer;
begin
  Result := False;
  if StrToVersion(ARegVersion, LVersion) then
  begin
    LRADStudioVersions := _GetRADStudioVersions;
    for I := 0 to GetArrayLength(LRADStudioVersions) - 1 do
    begin
      ARADStudioVersion := LRADStudioVersions[I];
      if StrToVersion(ARADStudioVersion.RegVersion, LRADVersion) and SamePackedVersion(LRADVersion, LVersion) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
  if not Result then
    Log(Format('RADStudio.TryGetRADStudioVersionByRegVersion: Failed to get the RAD Studio version by the reg version "%s".', [ARegVersion]));
end;

function TryGetRADStudioVersionOfProject(const AProject: TRADStudioProject; out ARADStudioVersion: TRADStudioVersion): Boolean;
var
  LProjVersion: Int64;
  LMaxVersion: Int64;
  LMinVersion: Int64;
  LRADStudioVersions: _TRADStudioVersions;
  I: Integer;
begin
  Result := False;
  if StrToVersion(AProject.ProjectVersion, LProjVersion) then
  begin
    LRADStudioVersions := _GetRADStudioVersions;
    for I := 0 to GetArrayLength(LRADStudioVersions) - 1 do
    begin
      ARADStudioVersion := LRADStudioVersions[I];
      if AProject.Personality = ppDelphi then
      begin
        if (not StrToVersion(ARADStudioVersion.MaxDprojVersion, LMaxVersion)) or
          (not StrToVersion(ARADStudioVersion.MinDprojVersion, LMinVersion)) then
        begin
          Continue;
        end;
        if (LProjVersion >= LMinVersion) and (LProjVersion <= LMaxVersion) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
  if not Result then
    Log(Format('RADStudio.TryGetRADStudioVersionOfProject: Failed to get the RAD Studio version of the project "%s" with project version "%s".', [AProject.FileName, AProject.ProjectVersion]));
end;

function GetKnownPackagesNodeName(const APlatform: TProjectPlatform): String;
begin
  if APlatform = pfWin64 then
    Result := '\Known Packages x64'
  else 
    Result := '\Known Packages';
end;

function TryRegisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ABplFileName, ADescription: string): Boolean;
var
  LNode: string;
begin
  LNode := GetKnownPackagesNodeName(APlatform);
  Result := TryUnregisterRADStudioBpl(ARADStudioVersion, APlatform, ABplFileName) and
    RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + LNode, ABplFileName, ADescription);
  if not Result then
    Log(Format('RADStudio.TryRegisterRADStudioBpl: Failed to register the RAD Studio bpl "%s".', [ABplFileName]));
end;

function TryRemoveFromGetIt(const AInfos: TRADStudioInfos; const ALibraryName: string): Boolean;
var
  I, J: Integer;
  LResultCode: Integer;
  LTempFileName: string;
  LResultLines: TStringList;
  LLineSplitted: TArrayOfString;
  LItemId: string;
  LLibrarySearchName: string;
begin
  Result := True;
  if ALibraryName = '' then
    Exit;
  Log(Format('RADStudio.TryRemoveFromGetIt: Trying to remove from GetIt the library "%s"', [ALibraryName]));
  if Pos('-', ALibraryName) = 0 then
    LLibrarySearchName := ALibraryName + '-'
  else
    LLibrarySearchName := ALibraryName;
  for I := 0 to GetArrayLength(AInfos) - 1 do
  begin
    if (AInfos[I].Status <> riNotFound) and AInfos[I].Version.HasGetItCmd then
    begin
      LTempFileName := AddBackslash(ExpandConstant('{tmp}')) + Format('getit_list_installed_%s_%s_output.txt', [ALibraryName, AInfos[I].Version.RegVersion]);
      DeleteFile(LTempFileName);
      if (not Exec(ExpandConstant('{cmd}'), Format('/C ""%sbin\GetItCmd.exe" ' + AInfos[I].Version.GetItCmdListInstalledParams + '>"%s""', [AddBackslash(AInfos[I].RootDir), LLibrarySearchName, LTempFileName]), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or
        (LResultCode <> 0) or (not FileExists(LTempFileName)) then
      begin
        Log(Format('RADStudio.TryRemoveFromGetIt: Cannot possible to list installed libraries with GetIt of "%s"', [AInfos[I].Version.Name]));
        Result := False;
        Continue;
      end;
      LResultLines := TStringList.Create;
      try
        LResultLines.LoadFromFile(LTempFileName);
        for J := 0 to LResultLines.Count - 1 do
        begin
          if StartsWithText(LResultLines[J], LLibrarySearchName) then
          begin
            LLineSplitted := SplitString(LResultLines[J], ' ');
            if GetArrayLength(LLineSplitted) > 0 then
            begin
              LItemId := LLineSplitted[0];
              if (not Exec(ExpandConstant('{cmd}'), Format('/C ""%sbin\GetItCmd.exe" ' + AInfos[I].Version.GetItCmdUninstallParams + '"', [AddBackslash(AInfos[I].RootDir), LItemId]), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or (LResultCode <> 0) then
              begin
                Log(Format('RADStudio.TryRemoveFromGetIt: Cannot possible to uninstall the library "%s" with GetIt of "%s"', [ALibraryName, AInfos[I].Version.Name]));
                Result := False;
              end;
            end;
          end;
        end;
      finally
        LResultLines.Free;
      end;
    end;
  end;
end;

function _TryRemoveRADStudioBplFromDisabledPackages(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean;
var
  LRegisteredBpls: TArrayOfString;
  LBplName: string;
  I: Integer;
begin
  Result := True;
  if RegGetValueNames(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Disabled Packages', LRegisteredBpls) then
  begin
    LBplName := ExtractFileName(ABplFileName);
    for I := 0 to GetArrayLength(LRegisteredBpls) - 1 do
    begin
      if SameText(LBplName, ExtractFileName(LRegisteredBpls[I])) and not RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Disabled Packages', LRegisteredBpls[I]) then
      begin
        Result := False;
        Log(Format('RADStudio._TryRemoveRADStudioBplFromDisabledPackages: Failed to delete the value "%s" from registry "%s".', [LRegisteredBpls[I], 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Disabled Packages']));
      end;
    end;
  end;
end;

function TryRemoveRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName: string): Boolean;
begin
  Result := (not RegValueExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName)) or
    RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName);
  if not Result then
    Log(Format('RADStudio.TryRemoveRADStudioEnvVariable: Failed to remove an environment variable to the RAD Studio registry (Name: "%s"; Registry: "%s").', [AName, 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables']));
end;

function TryRemoveRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Browsing Path', APath);
end;

function TryRemoveRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Debug DCU Path', APath);
end;

function _TryRemoveRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean;
var
  LNewValue: string;
  LPathsToRemove: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I: Integer;
begin
  Result := True;
  if RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue) and (LNewValue <> '') then
  begin
    LPathsToRemove := SplitString(APath, ';');
    LCurrentPaths := SplitString(LNewValue, ';');
    for I := 0 to GetArrayLength(LPathsToRemove) - 1 do
    begin
      LNewValue := LPathsToRemove[I];
      LCurrentPaths := RemoveString(LCurrentPaths, LNewValue, False);
      LCurrentPaths := RemoveString(LCurrentPaths, LNewValue + '\', False);
    end;
    LNewValue := JoinStrings(LCurrentPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue);
    if not Result then
      Log(Format('RADStudio._TryRemoveRADStudioLibraryPath: Failed to remove a library path to the RAD Studio registry (RegName: "%s", Path: "%s"). Failed to write the registry "%s"', [ARegName, APath, 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform)]));
  end
  else
    Log(Format('RADStudio._TryRemoveRADStudioLibraryPath: Failed to remove a library path to the RAD Studio registry (RegName: "%s", Path: "%s"). The registry name, does not exists in registry "%s".', [ARegName, APath, 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform)]));
end;

function TryRemoveRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Search Path', APath);
end;

function TryRemoveRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean;
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  if RegValueExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH') then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LValue := JoinStrings(LPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue);
    if not Result then
      Log(Format('RADStudio.TryRemoveRADStudioPathEnvVariable: Failed to remove a path environment variable to the RAD Studio registry (Path: "%s"). Cannot possible to write the value PATH in registry "%s"', [APath, 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables']));
  end
  else
  begin
    Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables');
    if not Result then
      Log(Format('RADStudio.TryRemoveRADStudioPathEnvVariable: Failed to remove a path environment variable to the RAD Studio registry (Path: "%s"). The registry "%s" does not exist.', [APath, 'HKEY_CURRENT_USER\' + GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables']));
  end;
end;

function TryUnregisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ABplFileName: string): Boolean;
var
  LRegisteredBpls: TArrayOfString;
  LBplName: string;
  I: Integer;
var
  LNode: string;
begin
  LNode := GetKnownPackagesNodeName(APlatform);
  Result := RegGetValueNames(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + LNode, LRegisteredBpls);
  if Result then
  begin
    LBplName := ExtractFileName(ABplFileName);
    for I := 0 to GetArrayLength(LRegisteredBpls) - 1 do
    begin
      if SameText(LBplName, ExtractFileName(LRegisteredBpls[I])) then
      begin
        if not RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + LNode, LRegisteredBpls[I]) then
        begin
          Result := False;
          Log(Format('RADStudio.TryUnregisterRADStudioBpl: Failed to unregister the bpl "%s". Cannot delete the value "%s" from registry "%s"', [ABplFileName, LRegisteredBpls[I], 'HKEY_CURRENT_USER' + GetRADStudioRegKey(ARADStudioVersion) + LNode]));
        end;
      end;  
    end;
  end
  else
    Log(Format('RADStudio.TryUnregisterRADStudioBpl: Failed to unregister the bpl "%s". Cannot possible to get the list of Known Packages from registry "%s"', [ABplFileName, 'HKEY_CURRENT_USER' + GetRADStudioRegKey(ARADStudioVersion) + LNode]));
  Result := _TryRemoveRADStudioBplFromDisabledPackages(ARADStudioVersion, ABplFileName) and Result;
end;

// end.
#endif
