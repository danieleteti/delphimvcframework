#ifndef UnitSetupMain
#define UnitSetupMain

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
// unit Setup.Main;

// interface

// implementation

// uses
  #include ".\RADStudio.inc"
  #include ".\RADStudio.Build.inc"
  #include ".\RADStudio.Project.inc"
  #include ".\IO.Utils.inc"
  #include ".\String.Utils.inc"
  #include ".\Setup.Pages.inc"
  #include ".\Setup.Style.inc"
  #include ".\Setup.Utils.inc"

type
  TGetBuildCustomParamEvent = function(): string;
  TTryPrepareProjectInstallationEvent = function(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
  TTryPrepareProjectUninstallationEvent = function(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;

/// <summary> [Event] Called to perform custom pre-install and post-install tasks </summary>
procedure CurStepChanged(ACurStep: TSetupStep); forward;
/// <summary> [Event] Called to perform custom pre-uninstall and post-uninstall tasks </summary>
procedure CurUninstallStepChanged(ACurUninstallStep: TUninstallStep); forward;
/// <summary> Get the default installation directory </summary>
function GetDefaultDirName(AParam: string): string; forward;
/// <summary> [Event] First event, triggered when setup initialized just in installation mode </summary>
function InitializeSetup: Boolean; forward;
/// <summary> [Event] Triggered when initialize wizard (after InitializeSetup) </summary>
procedure InitializeWizard; forward;
/// <summary> Install the packages </summary>
function InstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects; const ABeforeProjectBuild: TBeforeProjectBuildFunc): Boolean; forward;
/// <summary> Check if the setup can create the uninstall registry key to be able to uninstall in windows settings. This reg key can be disabled by executing the setup with the parameter /CreateUninstallRegKey=no </summary>
function NeedsUninstallRegKey: Boolean; forward;
/// <summary> [Event] Called to determine whether or not a particular page should be shown at all </summary>
function ShouldSkipPage(APageID: Integer): Boolean; forward;
/// <summary> Uninstall the packages </summary>
function UninstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean; forward;

var
  FGroupProjects: TRADStudioGroupProjects;
  FMaxVersionInGroupProjects: TRADStudioVersion;
  FOnBeforeProjectBuild: TBeforeProjectBuildFunc;
  FOnGetBuildCustomParam: TGetBuildCustomParamEvent;
  FOnTryExtractPreBuildObjects: TTryExtractPreBuildObjectsProc;
  FOnTryPrepareProjectInstallation: TTryPrepareProjectInstallationEvent;
  FOnTryPrepareProjectUninstallation: TTryPrepareProjectUninstallationEvent;
  FRADStudioInfos: TRADStudioInfos;

// implementation

/// <summary> Make project files (dproj, dpr, groupproj) for versions supported </summary>
procedure _MakeProjectsForVersionsSupported(var AGroupProjects: TRADStudioGroupProjects; const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion); forward;
/// <summary> Check if needs directory page by checking the define FilesEmbedded </summary>
function _NeedsDirPage: Boolean; forward;

procedure CurStepChanged(ACurStep: TSetupStep);
var
  LSelectedRADStudioVersions: TRADStudioInfos;
begin
  case ACurStep of
    ssInstall:
      begin
        LSelectedRADStudioVersions := GetSelectedRADStudioVersions;
        Log(Format('Start installation at folder "%s"...', [WizardDirValue]));
        #ifdef FilesEmbedded
        ReplaceRootPathOfRADStudioGroupProjects(ExpandConstant('{tmp}\') + '{app}', ExpandConstant('{app}'), FGroupProjects);
        #endif
        SetAbsolutePathsInRADStudioGroupProjects(ExpandConstant('{app}'), FGroupProjects);
        if IsUpgrade then
        begin
          WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingDetectedVersion');
          if NeedsUninstallRegKey then
          begin
            if (not IsVerySilent) and HaveRADStudioInstanceRunning then
            begin
              TryShowErrorEx(CustomMessage('SetupMainUninstallAbortedToCloseRADStudioInstance'), False);
              Abort;
            end;
            if not TryUninstallCurrentVersion then
            begin
              TryShowErrorFmt(CustomMessage('SetupMainCouldNotUninstallDetectedVersion'), ['{#LibraryName}']);
              Abort;
            end;
          end;
        end;
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingFromGetIt');
        TryRemoveFromGetIt(LSelectedRADStudioVersions, '{#LibraryName}');
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingPackages');
        if not UninstallPackages(LSelectedRADStudioVersions, FGroupProjects) then
        begin
          TryShowError(CustomMessage('SetupMainCouldNotUninstallPackages'));
          Abort;
        end;
      end;
    ssPostInstall:
      begin
        LSelectedRADStudioVersions := GetSelectedRADStudioVersions;
        if GetArrayLength(LSelectedRADStudioVersions) <> 0 then
        begin
          Log('Start installation post extraction files...');
          #ifdef FilesEmbedded
          CopyDirectory(ExpandConstant('{tmp}\') +'{app}\{#LibraryPackagesFolder}', ExpandConstant('{app}\') + '{#LibraryPackagesFolder}', False);
          #endif
          if not InstallPackages(GetSelectedRADStudioVersions, FGroupProjects, FOnBeforeProjectBuild) then
            Abort;
          if HaveRADStudioInstanceRunning and not IsSilent then
            ShowMessage(CustomMessage('SetupMainInstallationSuccesfullyRestartRADStudio'));
        end;
      end;
  else
  end;
end;

procedure CurUninstallStepChanged(ACurUninstallStep: TUninstallStep);
begin
  case ACurUninstallStep of
    usUninstall: ;
    usPostUninstall:
      begin
        SetAbsolutePathsInRADStudioGroupProjects(ExpandConstant('{app}'), FGroupProjects);
        if not UninstallPackages(FRADStudioInfos, FGroupProjects) then
        begin
          TryShowError(CustomMessage('SetupMainCouldNotUninstallPackages'));
          Abort;
        end;
      end;
  else
  end;
end;

function GetDefaultDirName(AParam: string): string;
begin
  if _NeedsDirPage then
    Result := ExpandConstant('{userdocs}\{#LibraryName}')
  else
    Result := GetCurrentDir;
end;

function InitializeSetup: Boolean;
var
  LPackagesPath: string;
begin
  Result := IsVerySilent or (not HaveRADStudioInstanceRunning);
  if not Result then
  begin
    TryShowMessage(CustomMessage('SetupMainInstallAbortedToCloseRADStudioInstance'));
    Exit;
  end;
  LPackagesPath := AddBackslash('{#LibraryPackagesFolder}');
  #ifdef FilesEmbedded
  LPackagesPath := AddBackslash(CombinePath('{app}', LPackagesPath));
  ExtractTemporaryFiles(LPackagesPath + '*');
  LPackagesPath := ExpandConstant('{tmp}\') + LPackagesPath;
  #else
  LPackagesPath := CombinePath(GetCurrentDir, LPackagesPath);
  #endif
  Result := TryGetRADStudioGroupProjectsFromPath(LPackagesPath, FGroupProjects);
  if Result then
  begin
    FRADStudioInfos := GetRADStudioInfosSupportedByGroupProjects(FGroupProjects);
    TryGetMaxRADStudioVersionInGroupProjects(FGroupProjects, FMaxVersionInGroupProjects);
    Result := GetArrayLength(FRADStudioInfos) > 0;
    if Result then
      _MakeProjectsForVersionsSupported(FGroupProjects, FRADStudioInfos, FMaxVersionInGroupProjects)
    else
      TryShowError(CustomMessage('SetupMainErrorNoRADStudioVersionAvailable'));
  end
  else
    TryShowError(CustomMessage('SetupMainErrorFailedToGetGroupProjects'));
end;

// Uninstall initialization
function InitializeUninstall: Boolean;
begin
  Result := IsVerySilent or (not HaveRADStudioInstanceRunning);
  if not Result then
  begin
    TryShowMessage(CustomMessage('SetupMainUninstallAbortedToCloseRADStudioInstance'));
    Exit;
  end;
  Result := TryGetRADStudioGroupProjectsFromPath(AddBackslash(CombinePath(ExpandConstant('{app}'), '{#LibraryPackagesFolder}')), FGroupProjects);
  if Result then
  begin
    FRADStudioInfos := GetRADStudioInfosSupportedByGroupProjects(FGroupProjects);
    TryGetMaxRADStudioVersionInGroupProjects(FGroupProjects, FMaxVersionInGroupProjects);
  end
  else
    TryShowError(CustomMessage('SetupMainErrorFailedToGetGroupProjects'));
end;

// Create wizard custom pages
procedure InitializeWizard;
begin
  CreateRADStudioVersionsChoicePage(wpWelcome, FRADStudioInfos, FMaxVersionInGroupProjects);
  WizardForm.LicenseAcceptedRadio.Checked := True;
end;

function InstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects;
  const ABeforeProjectBuild: TBeforeProjectBuildFunc): Boolean;
var
  I, J, K: Integer;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  
  LBplFileName: string;
  LProjectDir: string;
  LBplPrj32Dir: string;
  LBplPrj64Dir: string;
  LBplPrj64FileName: string;

  LBplOutputPath: string;

  LRadStudioCommon32Dir: string;
  LRadStudioCommon64Dir: string;
  LRadStudioCommon32FileName: string;
  LRadStudioCommon64FileName: string;
  LSearchPath: string;
  LDcpFileName: string;
  LPlatform: TProjectPlatform;
  LCustomParam: string;
  LBuilt: Boolean;
begin
  Log('Setup.Main.InstallPackages: Starting...');
  WizardForm.ProgressGauge.Min := 0;
  WizardForm.ProgressGauge.Max := GetTryBuildRADStudioPackagesSteps(ARADStudioInfos, AGroupProjects) + 1;
  WizardForm.ProgressGauge.Position := 0;
  if FOnGetBuildCustomParam <> nil then
    LCustomParam := FOnGetBuildCustomParam()
  else
    LCustomParam := '';
  Result := TryBuildRADStudioPackages(ARADStudioInfos, AGroupProjects, LCustomParam, FOnBeforeProjectBuild, FOnTryExtractPreBuildObjects);
  if Result then
  begin
    WizardForm.StatusLabel.Caption := CustomMessage('SetupMainInstallingPackages');
    try
      for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
      begin
        LInfo := ARADStudioInfos[I];
        if LInfo.Status in [riNotFound, riNeedOpenFirst] then
          Continue;
        Log(Format('Setup.Main.InstallPackages: Starting installation in %s...', [LInfo.Version.Name]));
        for J := 0 to GetArrayLength(AGroupProjects) - 1 do
        begin
          for K := GetArrayLength(AGroupProjects[J].Items) - 1 downto 0 do
          begin
            LProject := AGroupProjects[J].Items[K].Project;
            if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) then
            begin
              LVersion := LInfo.Version;
              Log(Format('Setup.Main.InstallPackages: Starting installation of package "%s" in "%s"...', [LProject.FileName, LVersion.Name]));

              if (FOnTryPrepareProjectInstallation <> nil) and
                not FOnTryPrepareProjectInstallation(AGroupProjects[J].Items[K], LInfo) then
              begin
                TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']);
                Result := False;
                Exit;
              end;
              LProject := AGroupProjects[J].Items[K].Project;

              LRadStudioCommon64Dir := GetRADStudioCommonBplDir(LInfo, pfWin64);
              LRadStudioCommon32Dir := GetRADStudioCommonBplDir(LInfo, pfWin32);
              
              LProjectDir := ExtractFilePath(LProject.FileName);
              LBplOutputPath := LProject.BplOutputPath;
              if not SameText(LProject.BplOutputPath, LProjectDir) then
              begin
                // The Bpl is built into a specific Project Folder: copy to RadStudioCommonBpl
                if not TryGetRADStudioProjectBplOutputFileName(LProject, LProject.DllSuffix, LBplFileName) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainCouldNotDetectProjectBplOutputFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]);
                  Result := False;
                  Exit;
                end;
                // The Bpl file is built in Project folder: copy the bpl file to RadStudioCommonBplDir
                //Copy always the 32 bit BPL file
                if (LRadStudioCommon32Dir <> '') then
                begin
                  CopyFileToDir(LBplFileName, LRadStudioCommon32Dir);
                  // Registering Bpl 32 Bit
                  if LProject.IsInstallable then
                  begin
                    if not TryRegisterRADStudioBpl(LVersion, pfWin32, LBplFileName, LProject.Description) then
                    begin
                      TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']);
                      Result := False;
                      Exit;
                    end;
                  end;  
                end;
                //Copy the 64 bit BPL file only if exists
                LBplPrj64FileName := ExpandProjectPath(LBplFileName, LInfo.BuildConfig, pfWin64);  
                if (pfWin64 in LProject.Platforms) and (LRadStudioCommon64Dir <> '') then
                begin
                  CopyFileToDir(LBplPrj64FileName, LRadStudioCommon64Dir);
                  // Registering Bpl 64 Bit
                  if LProject.IsInstallable then
                  begin
                    if not TryRegisterRADStudioBpl(LVersion, pfWin64, LBplFileName, LProject.Description) then
                    begin
                      TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']);
                      Result := False;
                      Exit;
                    end;
                  end;  
                end;
                // Registering Bpl
                if LProject.IsInstallable then
                begin
                  //Add Path Environment Variable
                  if not TryAddRADStudioPathEnvVariable(LVersion, RemoveBackslash(ExtractFilePath(LBplFileName))) then
                  begin
                    TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '4']);
                    Result := False;
                    Exit;
                  end;
                end;  
              end
              else
              begin
                // The Bpl file is built in RadStudioCommonBplDir: copy in LibraryPath\${Platform}\${Config}\Bpl
                //Acquire only bpl FileName with suffix expanded
                if not TryGetRADStudioBplFileName(LProject, LProject.DllSuffix, LBplFileName) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainCouldNotDetectProjectBplFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]);
                  Result := False;
                  Exit;
                end;
                //Check if the file is Built before proceeding to Installation
                LBuilt := True;
                if FOnBeforeProjectBuild <> nil then
                  LBuilt := FOnBeforeProjectBuild(LProject, LPlatform, LInfo);
                if not LBuilt then
                  Continue;
                LRadStudioCommon32FileName := AddBackSlash(LRadStudioCommon32Dir) + LBplFileName; 
                if not FileExists(LRadStudioCommon32FileName) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [LRadStudioCommon32FileName, LVersion.Name, '2']);
                  Result := False;
                  Exit;
                end
                else
                begin
                  //Copy the 32 bit BPL file from RadStudioCommonBplDir Folder to Project folder
                  LBplPrj32Dir := ExpandProjectPath(LProjectDir+'$(Platform)\$(Config)\Bpl', LInfo.BuildConfig, pfWin32);
                  if (not LProject.IsDesignOnly) and (LRadStudioCommon32Dir <> '') then
                    CopyFileToDir(LRadStudioCommon32FileName, LBplPrj32Dir);
                  // Registering 32Bit Bpl
                  if LProject.IsInstallable then
                  begin
                    if not TryRegisterRADStudioBpl(LVersion, pfWin32, LRadStudioCommon32FileName, LProject.Description) then
                    begin
                      TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']);
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
                //Copy the 64 bit BPL file only if exists
                LRadStudioCommon64FileName := AddBackSlash(LRadStudioCommon64Dir) + LBplFileName; 
                if FileExists(LRadStudioCommon64FileName) and (pfWin64 in LProject.Platforms) then
                begin
                  LBplPrj64Dir := ExpandProjectPath(LProjectDir+'$(Platform)\$(Config)\Bpl', LInfo.BuildConfig, pfWin64);
                  CopyFileToDir(LRadStudioCommon64FileName, LBplPrj64Dir);
                  // Registering 64Bit Bpl
                  if LProject.IsInstallable and (LBplPrj64Dir <> '') then
                  begin
                    if not TryRegisterRADStudioBpl(LVersion, pfWin64, LRadStudioCommon64FileName, LProject.Description) then
                    begin
                      TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']);
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
              end;
              
              // Adding search paths of Sources from LProject.SourcePaths at the Bottom
              for LPlatform := LowProjectPlatform to HighProjectPlatform do
              begin
                if (not (LPlatform in LProject.Platforms)) or (not CheckIfRADStudioSupportsPlatform(LInfo, LPlatform)) then
                  Continue;
                if LPlatform in GetPlatformsAllowedToBuild(LInfo) then
                begin
                  if Length(LProject.SourcePaths) > 0 then
                  begin
                    LSearchPath := JoinStrings(LProject.SourcePaths, ';', False);  
                    //Add Source Search Path on Top from second position
                    TryAddRADStudioLibrarySearchPath(LVersion, LPlatform, LSearchPath, 1);
                  end;  
                  // Adding search paths, from LProject.DCUOutputPath at position before first Source Path
                  LDcpFileName := GetRADStudioProjectDcpFileName(LProject);
                  LDcpFileName := AddBackSlash(GetRADStudioCommonDcpDir(LInfo, LPlatform)) + LDcpFileName;
                  if FileExists(LDcpFileName) then
                  begin
                    LSearchPath := ExpandProjectPath(LProject.DCUOutputPath, LInfo.BuildConfig, LPlatform);
                    //Add Binary Search Path on Top in first position
                    TryAddRADStudioLibrarySearchPath(LVersion, LPlatform, LSearchPath, 0);
                  end;  
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      if not Result then
      begin
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainRevertingPackagesInstallationAfterFailure');
        Log(CustomMessage('SetupMainRevertingPackagesInstallationAfterFailure'));
        UninstallPackages(ARADStudioInfos, AGroupProjects);
      end;
    end;
  end;
  WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Max;
end;

procedure _MakeProjectsForVersionsSupported(var AGroupProjects: TRADStudioGroupProjects;
  const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion);
var
  I, J, K, L, M: Integer;
  LNewGroupProject: TRADStudioGroupProject;
  LNewPath: string;
  LFiles: TArrayOfString;
begin
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    if CompareRADStudioVersions(ARADStudioInfos[I].Version, AMaxVersionOfficiallySupported) > 0 then
    begin
      for J := 0 to GetArrayLength(AGroupProjects) - 1 do
      begin
        if ContainsString(SplitString(AGroupProjects[J].FileName, '\'), AMaxVersionOfficiallySupported.Name, False) then
        begin
          LNewPath := ExtractFilePath(ReplaceFolderNameInPath(AGroupProjects[J].FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False));
          if CopyDirectory(ExtractFilePath(AGroupProjects[J].FileName), LNewPath, True) then
          begin
            LFiles := GetFiles(LNewPath, '*', soAllDirectories);
            for K := 0 to GetArrayLength(LFiles) - 1 do
              ReplaceStringInFile(LFiles[K], AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            LNewGroupProject := CopyRADStudioGroupProject(AGroupProjects[J]);
            LNewGroupProject.FileName := ReplaceFolderNameInPath(LNewGroupProject.FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            for L := 0 to GetArrayLength(LNewGroupProject.Items) - 1 do
            begin
              LNewGroupProject.Items[L].Project.FileName := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              ReplaceStringInFile(LNewGroupProject.Items[L].Project.FileName,
                Format('<ProjectVersion>%s</ProjectVersion>', [LNewGroupProject.Items[L].Project.ProjectVersion]),
                Format('<ProjectVersion>%s</ProjectVersion>', [ARADStudioInfos[I].Version.MaxDprojVersion]), False);
              LNewGroupProject.Items[L].Project.ProjectVersion := ARADStudioInfos[I].Version.MaxDprojVersion;
              LNewGroupProject.Items[L].Project.BplOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.BplOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              LNewGroupProject.Items[L].Project.DcpOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.DcpOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              LNewGroupProject.Items[L].Project.DCUOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.DCUOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              for M := 0 to GetArrayLength(LNewGroupProject.Items[L].Project.SourcePaths) - 1 do
                LNewGroupProject.Items[L].Project.SourcePaths[M] := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.SourcePaths[M], AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            end;
            SetArrayLength(AGroupProjects, GetArrayLength(AGroupProjects) + 1);
            AGroupProjects[GetArrayLength(AGroupProjects) - 1] := LNewGroupProject;
          end;
        end;
      end;
    end;
  end;

  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
    for J := 0 to GetArrayLength(AGroupProjects[I].Items) - 1 do
      Log(Format('_MakeProjectsForVersionsSupported: f "%s" g "%s"...', [AGroupProjects[I].Items[J].Project.FileName, AGroupProjects[I].FileName]));
end;

function _NeedsDirPage: Boolean;
begin
  #ifdef FilesEmbedded
  Result := True
  #else
  Result := False;
  #endif
end;

function NeedsUninstallRegKey: Boolean;
begin
  #ifdef FilesEmbedded
  Result := StrToBoolDef(ExpandConstant('{param:CreateUninstallRegKey|yes}'), True);
  #else
  Result := False;
  #endif
end;

function ShouldSkipPage(APageID: Integer): Boolean;
begin
  Result := (APageID = wpSelectDir) and not _NeedsDirPage;
end;

function UninstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean;
var
  I, J, K, L: Integer;
  LBplFileName, LOldBplFileName, LDcpFileName: string;
  LConfig: TProjectConfig;
  LFileName, LProjectDir: string;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LPlatform: TProjectPlatform;
  LReleaseSearchPath: string;
  LDebugSearchPath: string;
  LSearchPath: string;
  LRadStudioCommon32BplDir: string;
  LRadStudioCommon64BplDir: string;
  LRadStudioCommon32DcpDir: string;
  LRadStudioCommon64DcpDir: string;
  LBplOutputPath: string;
  LRadStudioCommon32FileName: string;
  LRadStudioCommon64FileName: string;  
begin
  Result := True;
  Log('Setup.Main.UninstallPackages: Starting...');
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    LInfo := ARADStudioInfos[I];
    if LInfo.Status in [riNotFound, riNeedOpenFirst] then
      Continue;

    LRadStudioCommon64BplDir := GetRADStudioCommonBplDir(LInfo, pfWin64);
    LRadStudioCommon32BplDir := GetRADStudioCommonBplDir(LInfo, pfWin32);
    LRadStudioCommon64DcpDir := GetRADStudioCommonDcpDir(LInfo, pfWin64);
    LRadStudioCommon32DcpDir := GetRADStudioCommonDcpDir(LInfo, pfWin32);
    for J := 0 to GetArrayLength(AGroupProjects) - 1 do
    begin
      for K := 0 to GetArrayLength(AGroupProjects[J].Items) - 1 do
      begin
        LProject := AGroupProjects[J].Items[K].Project;
        if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) then
        begin
          LVersion := LInfo.Version;
          Log(Format('Setup.Main.UninstallPackages: Starting uninstallation of package "%s" in "%s"...', [LProject.FileName, LVersion.Name]));

          if TryGetRADStudioBplFileName(LProject, LProject.DllSuffix, LBplFileName) then
          begin
            //Delete bpl files in RADStudioCommonBplDir
            DeleteFile(CombinePath(LRadStudioCommon32BplDir, ExtractFileName(LBplFileName)));
            DeleteFile(CombinePath(LRadStudioCommon64BplDir, ExtractFileName(LBplFileName)));
          end;
 
          //Delete dcp files in RADStudioCommonDcpDir 32bit
          LDcpFileName := GetRADStudioProjectDcpFileName(LProject);
          DeleteFile(CombinePath(LRadStudioCommon32DcpDir, ExtractFileName(LDcpFileName)));
          DeleteFile(CombinePath(LRadStudioCommon64DcpDir, ExtractFileName(LDcpFileName)));
 
          if TryGetRADStudioBplFileName(LProject, LInfo.Version.OldLibSuffix, LOldBplFileName) then
          begin
            if (LOldBplFileName <> LBplFileName) then
            begin
              //Delete bpl with old suffix (eg.D10_3) files in RADStudioCommonBplDir if different of Actual
              if LRadStudioCommon32BplDir <> '' then
                DeleteFile(CombinePath(LRadStudioCommon32BplDir, ExtractFileName(LOldBplFileName)));
              if LRadStudioCommon64BplDir <> '' then
                DeleteFile(CombinePath(LRadStudioCommon64BplDir, ExtractFileName(LOldBplFileName)));
            end;    
          end;
 
          if TryGetRADStudioBplFileName(LProject, '_'+LInfo.Version.OldLibSuffix, LOldBplFileName) then
          begin
            if (LOldBplFileName <> LBplFileName) then
            begin
              //Delete bpl with old suffix (eg._D10_3) files in RADStudioCommonBplDir if different of Actual
              if LRadStudioCommon32BplDir <> '' then
                DeleteFile(CombinePath(LRadStudioCommon32BplDir, ExtractFileName(LOldBplFileName)));
              if LRadStudioCommon64BplDir <> '' then
                DeleteFile(CombinePath(LRadStudioCommon64BplDir, ExtractFileName(LOldBplFileName)));
            end;    
          end;
 
          LProjectDir := ExtractFilePath(LProject.FileName);
          LBplOutputPath := LProject.BplOutputPath;
          if not SameText(LProject.BplOutputPath, LProjectDir) then
          begin
            // The Bpl is built into a specific Project Folder: Unregister this bpl
            TryGetRADStudioProjectBplOutputFileName(LProject, LProject.DllSuffix, LBplFileName);
            for LConfig := LowProjectConfig to HighProjectConfig do
            begin
              // Unregistering Bpl 32Bit         
              LFileName := ExpandProjectPath(LBplFileName, LConfig, pfWin32);
              if not TryUnregisterRADStudioBpl(LVersion, pfWin32, LFileName) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                //Result := False;
              end;
              // Unregistering Bpl 64Bit
              LFileName := ExpandProjectPath(LBplFileName, LConfig, pfWin64);
              if not TryUnregisterRADStudioBpl(LVersion, pfWin64, LFileName) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                //Result := False;
              end;
            end;
          end
          else
          begin  
            // The Bpl file is built in RadStudioCommonBplDir: copy in LibraryPath\${Platform}\${Config}\Bpl
            //Acquire only bpl FileName with suffix expanded
            TryGetRADStudioBplFileName(LProject, LProject.DllSuffix, LBplFileName);
            LRadStudioCommon32FileName := AddBackSlash(LRadStudioCommon32BplDir) + LBplFileName;
            LRadStudioCommon64FileName := AddBackSlash(LRadStudioCommon64BplDir) + LBplFileName;
            for LConfig := LowProjectConfig to HighProjectConfig do
            begin
              // Unregistering Bpl 32Bit         
              if not TryUnregisterRADStudioBpl(LVersion, pfWin32, LRadStudioCommon32FileName) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                //Result := False;
              end;
              // Unregistering Bpl 64Bit         
              if not TryUnregisterRADStudioBpl(LVersion, pfWin64, LRadStudioCommon64FileName) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                //Result := False;
              end;
            end;
            //Try to unregister old Bpl suffix
            TryGetRADStudioBplFileName(LProject, LInfo.Version.OldLibSuffix, LOldBplFileName);
            LRadStudioCommon32FileName := AddBackSlash(LRadStudioCommon32BplDir) + LOldBplFileName; 
            if LOldBplFileName <> LBplFileName then
            begin
              for LConfig := LowProjectConfig to HighProjectConfig do
              begin
                // Unregistering Bpl 32Bit
                if not TryUnregisterRADStudioBpl(LVersion, pfWin32, LRadStudioCommon32FileName) then
                begin
                  Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                end;
                // Unregistering Bpl 64Bit
                if not TryUnregisterRADStudioBpl(LVersion, pfWin64, LRadStudioCommon64FileName) then
                begin
                  Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                end;
              end;
            end;  
          end;          
          if (FOnTryPrepareProjectUninstallation <> nil) and not FOnTryPrepareProjectUninstallation(AGroupProjects[J].Items[K], LInfo) then
          begin
            Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']));
            Result := False;
          end;
          LProject := AGroupProjects[J].Items[K].Project;

          // Removing search paths
          for LPlatform := LowProjectPlatform to HighProjectPlatform do
          begin
            if (not (LPlatform in LProject.Platforms)) or (not CheckIfRADStudioSupportsPlatform(LInfo, LPlatform)) then
              Continue;
            if LPlatform in GetPlatformsAllowedToBuild(LInfo) then
            begin
              LReleaseSearchPath := ExpandProjectPath(LProject.DCUOutputPath, pcRelease, LPlatform);
              LDebugSearchPath := ExpandProjectPath(LProject.DCUOutputPath, pcDebug, LPlatform);
              if (not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, LReleaseSearchPath)) or
                (not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, LDebugSearchPath)) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '4']));
                Result := False;
              end;
            end;
            for L := 0 to GetArrayLength(LProject.SourcePaths) - 1 do
            begin
              LSearchPath := LProject.SourcePaths[L];
              if not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, LSearchPath) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '5']));
                Result := False;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

// end.
#endif
