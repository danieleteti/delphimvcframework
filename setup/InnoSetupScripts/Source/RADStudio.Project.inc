#ifndef UnitRADStudioProject
#define UnitRADStudioProject

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
// unit RADStudio.Project;

// interface

type
  TProjectPlatform = (pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice32, pfiOSDevice64, pfiOSSimARM64, pfiOSSimulator, pfOSX32, pfOSX64, pfOSXARM64, pfLinux64);
  TProjectPlatforms = set of TProjectPlatform;
  TProjectConfig = (pcDebug, pcRelease);
  TProjectConfigs = set of TProjectConfig;
  TProjectPersonality = (ppCBuilder, ppDelphi);

  TRADStudioProject = record
    FileName: string;
    Description: string;
    SourceFiles: TArrayOfString;
    SourcePaths: TArrayOfString;
    BplOutputPath: string;
    DcpOutputPath: string;
    DCUOutputPath: string;
    DllSuffix: string;
    Platforms: TProjectPlatforms;
    ProjectVersion: string;
    Personality: TProjectPersonality;
    IsDesignOnly: Boolean;
    IsPackage: Boolean;
    IsInstallable: Boolean;
    RequiresDCPCount: Integer;
  end;

  TRADStudioGroupProjectItem = record
    Project: TRADStudioProject;
    Configs: TProjectConfigs;
    Platforms: TProjectPlatforms;
  end;

  TRADStudioGroupProjectItems = array of TRADStudioGroupProjectItem;

  TRADStudioGroupProject = record
    FileName: string;
    Items: TRADStudioGroupProjectItems;
  end;

  TRADStudioGroupProjects = array of TRADStudioGroupProject;

/// <summary> Copy group project record </summary>
function CopyRADStudioGroupProject(const AGroupProject: TRADStudioGroupProject): TRADStudioGroupProject; forward;
/// <summary> Expand project path (directory or filename), expanding relative path, config and platform names </summary>
function ExpandProjectPath(const APath: string; const AConfig: TProjectConfig; const APlatform: TProjectPlatform): string; forward;
/// <summary> Get config name </summary>
function GetProjectConfigName(const AConfig: TProjectConfig): string; forward;
/// <summary> Get platform friendly name </summary>
function GetProjectPlatformFriendlyName(const APlatform: TProjectPlatform): string; forward;
/// <summary> Name of the platform used in library registry </summary>
function GetProjectPlatformLibraryName(const APlatform: TProjectPlatform): string; forward;
/// <summary> Get platform name </summary>
function GetProjectPlatformName(const APlatform: TProjectPlatform): string; forward;
/// <summary> Get the default DCP output Path + dcp filename of a RAD Studio project </summary>
function GetRADStudioProjectDcpFileName(const AProject: TRADStudioProject): string; forward;
/// <summary> Get the default DCP output Path + dcp filename of a RAD Studio project </summary>
function GetRADStudioProjectDcpOutputFileName(const AProject: TRADStudioProject): string; forward;
/// <summary> Replace the root path in all path/filename fields of RAD Studio group projects </summary>
procedure ReplaceRootPathOfRADStudioGroupProjects(const AOldRootPath, ANewRootPath: string; var AGroupProjects: TRADStudioGroupProjects); forward;
/// <summary> Change all paths/filename to absolute (removing relative paths) </summary>
procedure SetAbsolutePathsInRADStudioGroupProjects(const ADefaultRootPath: string; var AGroupProjects: TRADStudioGroupProjects); forward;
/// <summary> Try list and load all group projects from a path </summary>
function TryGetRADStudioGroupProjectsFromPath(const APath: string; out AGroupProjects: TRADStudioGroupProjects): Boolean; forward;
/// <summary> Try to load a RAD Studio group project </summary>
function TryLoadRADStudioGroupProject(const AFileName: string; out AGroupProject: TRADStudioGroupProject): Boolean; forward;
/// <summary> Try to load a RAD Studio project </summary>
function TryLoadRADStudioProject(const AFileName: string; out AProject: TRADStudioProject): Boolean; forward;
/// <summary> Try convert text to config </summary>
function TryStrToProjectConfig(const AConfigName: string; out AConfig: TProjectConfig): Boolean; forward;
/// <summary> Try convert text (with configs names separeted by ;) to configs </summary>
function TryStrToProjectConfigs(const AText: string; out AConfigs: TProjectConfigs): Boolean; forward;
/// <summary> Try convert text to platform </summary>
function TryStrToProjectPlatform(const APlatformName: string; out APlatform: TProjectPlatform): Boolean; forward;
/// <summary> Try convert text (with platforms names separeted by ;) to platforms </summary>
function TryStrToProjectPlatforms(const AText: string; out APlatforms: TProjectPlatforms): Boolean; forward;

const
  LowProjectPlatform = pfWin32;
  HighProjectPlatform = pfLinux64;
  LowProjectConfig = pcDebug;
  HighProjectConfig = pcRelease;

// implementation

// uses
  #include ".\IO.Utils.inc"
  #include ".\String.Utils.inc"
  #include ".\Setup.Utils.inc"

/// <summary> Expand just the project root, adding a ADefaultRootPath if it doesn't have and expanding the filename to remove relative paths </summary>
procedure _ExpandProjectRootPath(var APath: string; const ADefaultRootPath: string); forward;
/// <summary> Ascendent sort of items of group project by RequiresDCPCount </summary>
function _SortGroupProjectItems(AItems: TRADStudioGroupProjectItems): TRADStudioGroupProjectItems; forward;

function CopyRADStudioGroupProject(const AGroupProject: TRADStudioGroupProject): TRADStudioGroupProject;
var
  I, J: Integer;
begin
  Result.FileName := AGroupProject.FileName;
  SetArrayLength(Result.Items, GetArrayLength(AGroupProject.Items));
  for I := 0 to GetArrayLength(Result.Items) - 1 do
  begin
    Result.Items[I].Project.FileName := AGroupProject.Items[I].Project.FileName;
    Result.Items[I].Project.Description := AGroupProject.Items[I].Project.Description;
    Result.Items[I].Project.BplOutputPath := AGroupProject.Items[I].Project.BplOutputPath;
    Result.Items[I].Project.DcpOutputPath := AGroupProject.Items[I].Project.DcpOutputPath;
    Result.Items[I].Project.DCUOutputPath := AGroupProject.Items[I].Project.DCUOutputPath;
    Result.Items[I].Project.DllSuffix := AGroupProject.Items[I].Project.DllSuffix;
    Result.Items[I].Project.Platforms := AGroupProject.Items[I].Project.Platforms;
    Result.Items[I].Project.ProjectVersion := AGroupProject.Items[I].Project.ProjectVersion;
    Result.Items[I].Project.Personality := AGroupProject.Items[I].Project.Personality;
    Result.Items[I].Project.IsDesignOnly := AGroupProject.Items[I].Project.IsDesignOnly;
    Result.Items[I].Project.IsPackage := AGroupProject.Items[I].Project.IsPackage;
    Result.Items[I].Project.IsInstallable := AGroupProject.Items[I].Project.IsInstallable;
    Result.Items[I].Project.RequiresDCPCount := AGroupProject.Items[I].Project.RequiresDCPCount;
    SetArrayLength(Result.Items[I].Project.SourceFiles, GetArrayLength(AGroupProject.Items[I].Project.SourceFiles));
    for J := 0 to GetArrayLength(AGroupProject.Items[I].Project.SourceFiles) - 1 do
      Result.Items[I].Project.SourceFiles[J] := AGroupProject.Items[I].Project.SourceFiles[J];
    SetArrayLength(Result.Items[I].Project.SourcePaths, GetArrayLength(AGroupProject.Items[I].Project.SourcePaths));
    for J := 0 to GetArrayLength(AGroupProject.Items[I].Project.SourcePaths) - 1 do
      Result.Items[I].Project.SourcePaths[J] := AGroupProject.Items[I].Project.SourcePaths[J];
    Result.Items[I].Configs := AGroupProject.Items[I].Configs;
    Result.Items[I].Platforms := AGroupProject.Items[I].Platforms;
  end;
end;

function ExpandProjectPath(const APath: string; const AConfig: TProjectConfig; const APlatform: TProjectPlatform): string;
begin
  Result := APath;
  StringChangeEx(Result, '$(Platform)', GetProjectPlatformName(APlatform), True);
  StringChangeEx(Result, '$(Config)', GetProjectConfigName(AConfig), True);
  Result := ExpandConstant(Result);
  if Pos('$(', Result) = 0 then
    Result := ExpandFileName(Result);
end;

procedure _ExpandProjectRootPath(var APath: string; const ADefaultRootPath: string);
begin
  if ExtractFileDrive(APath) = '' then
    APath := CombinePath(ADefaultRootPath, APath);
  APath := ExpandFileName(APath);
end;

function GetProjectConfigName(const AConfig: TProjectConfig): string;
begin
  case AConfig of
    pcDebug  : Result := 'Debug';
    pcRelease: Result := 'Release';
  else
    Result := '';
  end;
end;

function GetProjectPlatformFriendlyName(const APlatform: TProjectPlatform): string;
begin
  case APlatform of
    pfWin32       : Result := 'Windows 32-bit';
    pfWin64       : Result := 'Windows 64-bit';
    pfAndroid     : Result := 'Android 32-bit';
    pfAndroid64   : Result := 'Android 64-bit';
    pfiOSDevice32 : Result := 'iOS Device 32-bit';
    pfiOSDevice64 : Result := 'iOS Device 64-bit';
    pfiOSSimARM64 : Result := 'iOS Simulator ARM 64-bit';
    pfiOSSimulator: Result := 'iOS Simulator';
    pfOSX32       : Result := 'OSX 32-bit';
    pfOSX64       : Result := 'OSX 64-bit';
    pfOSXARM64    : Result := 'OSX ARM 64-bit';
    pfLinux64     : Result := 'Linux 64-bit';
  else
    Result := '';
  end;
end;

function GetProjectPlatformLibraryName(const APlatform: TProjectPlatform): string;
begin
  if APlatform = pfAndroid then
    Result := 'Android32'
  else if APlatform = pfiOSDevice32 then
    Result := 'iOSDevice'
  else
    Result := GetProjectPlatformName(APlatform);
end;

function GetProjectPlatformName(const APlatform: TProjectPlatform): string;
begin
  case APlatform of
    pfWin32       : Result := 'Win32';
    pfWin64       : Result := 'Win64';
    pfAndroid     : Result := 'Android';
    pfAndroid64   : Result := 'Android64';
    pfiOSDevice32 : Result := 'iOSDevice32';
    pfiOSDevice64 : Result := 'iOSDevice64';
    pfiOSSimARM64 : Result := 'iOSSimARM64';
    pfiOSSimulator: Result := 'iOSSimulator';
    pfOSX32       : Result := 'OSX32';
    pfOSX64       : Result := 'OSX64';
    pfOSXARM64    : Result := 'OSXARM64';
    pfLinux64     : Result := 'Linux64';
  else
    Result := '';
  end;
end;

function GetRADStudioProjectDcpFileName(const AProject: TRADStudioProject): string;
begin
  Result := ExtractFileName(ChangeFileExt(AProject.FileName, '.dcp'));
end;

function GetRADStudioProjectDcpOutputFileName(const AProject: TRADStudioProject): string;
begin
  Result := AddBackslash(AProject.DcpOutputPath) + GetRADStudioProjectDcpFileName(AProject);
end;

procedure ReplaceRootPathOfRADStudioGroupProjects(const AOldRootPath, ANewRootPath: string; var AGroupProjects: TRADStudioGroupProjects);
var
  I, J, K: Integer;
  LGroupProject: TRADStudioGroupProject;
  LGroupProjectItem: TRADStudioGroupProjectItem;
  LProject: TRADStudioProject;
begin
  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
  begin
    LGroupProject := AGroupProjects[I];
    StringChangeEx(LGroupProject.FileName, AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
    for J := 0 to GetArrayLength(LGroupProject.Items) - 1 do
    begin
      LGroupProjectItem := LGroupProject.Items[J];
      LProject := LGroupProjectItem.Project;
      StringChangeEx(LProject.FileName, AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      for K := 0 to GetArrayLength(LProject.SourceFiles) - 1 do
        StringChangeEx(LProject.SourceFiles[K], AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      for K := 0 to GetArrayLength(LProject.SourcePaths) - 1 do
        StringChangeEx(LProject.SourcePaths[K], AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      StringChangeEx(LProject.BplOutputPath, AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      StringChangeEx(LProject.DcpOutputPath, AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      StringChangeEx(LProject.DCUOutputPath, AddBackslash(AOldRootPath), AddBackslash(ANewRootPath), True);
      LGroupProjectItem.Project := LProject;
      LGroupProject.Items[J] := LGroupProjectItem;
    end;
    AGroupProjects[I] := LGroupProject;
  end;
end;

procedure SetAbsolutePathsInRADStudioGroupProjects(const ADefaultRootPath: string; var AGroupProjects: TRADStudioGroupProjects);
var
  I, J, K: Integer;
  LGroupProject: TRADStudioGroupProject;
  LGroupProjectItem: TRADStudioGroupProjectItem;
  LGroupProjectPath: string;
  LProject: TRADStudioProject;
  LProjectPath: string;
begin
  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
  begin
    LGroupProject := AGroupProjects[I];
    _ExpandProjectRootPath(LGroupProject.FileName, ADefaultRootPath);
    LGroupProjectPath := ExtractFilePath(LGroupProject.FileName);
    for J := 0 to GetArrayLength(LGroupProject.Items) - 1 do
    begin
      LGroupProjectItem := LGroupProject.Items[J];
      LProject := LGroupProjectItem.Project;
      _ExpandProjectRootPath(LProject.FileName, LGroupProjectPath);
      LProjectPath := ExtractFilePath(LProject.FileName);
      for K := 0 to GetArrayLength(LProject.SourceFiles) - 1 do
        _ExpandProjectRootPath(LProject.SourceFiles[K], LProjectPath);
      for K := 0 to GetArrayLength(LProject.SourcePaths) - 1 do
        _ExpandProjectRootPath(LProject.SourcePaths[K], LProjectPath);
      _ExpandProjectRootPath(LProject.BplOutputPath, LProjectPath);
      _ExpandProjectRootPath(LProject.DcpOutputPath, LProjectPath);
      _ExpandProjectRootPath(LProject.DCUOutputPath, LProjectPath);
      LGroupProjectItem.Project := LProject;
      LGroupProject.Items[J] := LGroupProjectItem;
    end;
    AGroupProjects[I] := LGroupProject;
  end;
end;

function _SortGroupProjectItems(AItems: TRADStudioGroupProjectItems): TRADStudioGroupProjectItems;
var
  I: Integer;
  J: Integer;
  LMinIndex: Integer;
  LJProject, LMinProject: TRADStudioProject; 
  LProject: TRADStudioProject;
  LResult: TRADStudioGroupProjectItems;
begin
  SetArrayLength(LResult, GetArrayLength(AItems));
  for I := 0 to GetArrayLength(AItems) - 1 do
  begin
    LMinIndex := I;
    for J := 0 to GetArrayLength(AItems) - 1 do
    begin
      LJProject := AItems[J].Project;
      LMinProject := AItems[LMinIndex].Project;
      if (LJProject.RequiresDCPCount <> -1) and ((LJProject.RequiresDCPCount < LMinProject.RequiresDCPCount) or
        ((LJProject.RequiresDCPCount = LMinProject.RequiresDCPCount) and (CompareText(LJProject.FileName, LMinProject.FileName) < 0)) or
        (LMinProject.RequiresDCPCount = -1)) then
      begin
        LMinIndex := J;
      end;
    end;
    LResult[I] := AItems[LMinIndex];
    LProject := AItems[LMinIndex].Project;
    LProject.RequiresDCPCount := -1;
    AItems[LMinIndex].Project := LProject;
  end;
  Result := LResult;
end;

function TryGetRADStudioGroupProjectsFromPath(const APath: string; out AGroupProjects: TRADStudioGroupProjects): Boolean;
var
  LGroupProjectFiles: TArrayOfString;
  LGroupProject: TRADStudioGroupProject;
  I: Integer;
begin
  Result := True;
  AGroupProjects := [];
  LGroupProjectFiles := GetFiles(APath, '*.groupproj', soAllDirectories);
  for I := 0 to GetArrayLength(LGroupProjectFiles) - 1 do
  begin
    if not TryLoadRADStudioGroupProject(LGroupProjectFiles[I], LGroupProject) then
    begin
      Result := False;
      Exit;
    end;
    SetArrayLength(AGroupProjects, GetArrayLength(AGroupProjects) + 1);
    AGroupProjects[GetArrayLength(AGroupProjects) - 1] := LGroupProject;
  end;
end;

function TryLoadRADStudioGroupProject(const AFileName: string; out AGroupProject: TRADStudioGroupProject): Boolean;
var
  LItem: TRADStudioGroupProjectItem;
  LXMLNodeList: Variant;
  LXMLDocument: Variant;
  LAttributeValue: string;
  I: Integer;
begin
  Result := FileExists(AFileName);
  if Result then
  begin
    AGroupProject.FileName := AFileName;

    LXMLDocument := CreateOleObject('Msxml2.DOMDocument');
    try
      LXMLDocument.Async := False;
      LXMLDocument.ResolveExternals := False;
      LXMLDocument.Load(AFileName);
      if LXMLDocument.ParseError.ErrorCode <> 0 then
      begin
        TryShowErrorFmt(CustomMessage('RADStudioProjectErrorParsingGroupProject'), [AFileName, IntToStr(LXMLDocument.ParseError.ErrorCode), IntToStr(LXMLDocument.ParseError.Line), IntToStr(LXMLDocument.ParseError.LinePos), LXMLDocument.ParseError.Reason]);
        Result := False;
        Exit;
      end;

      // Items
      LXMLNodeList := LXMLDocument.SelectNodes('/Project/ItemGroup/BuildGroupProject[@Include and Enabled="True"]');
      AGroupProject.Items := [];
      for I := 0 to LXMLNodeList.Length-1 do
      begin
        if TryGetXMLNodeAttribute(LXMLNodeList.Item[I], 'Include', LAttributeValue) then
        begin
          LAttributeValue := ExtractFilePath(AGroupProject.FileName) + LAttributeValue;
          if TryStrToProjectConfigs(ReadXMLNodeText(LXMLNodeList.Item[I], 'Configurations', ''), LItem.Configs) and
            TryStrToProjectPlatforms(ReadXMLNodeText(LXMLNodeList.Item[I], 'Platforms', ''), LItem.Platforms) and
            TryLoadRADStudioProject(LAttributeValue, LItem.Project) then
          begin
            SetArrayLength(AGroupProject.Items, GetArrayLength(AGroupProject.Items) + 1);
            AGroupProject.Items[GetArrayLength(AGroupProject.Items) - 1] := LItem;
          end;
        end;
      end;
      //Sort Projects inside a Group Project by Requires Dcu count
      //AGroupProject.Items := _SortGroupProjectItems(AGroupProject.Items);
    except
      TryShowErrorFmt(CustomMessage('RADStudioProjectErrorUnexpectedParsingGroupProject'), [AFileName, GetExceptionMessage]);
      Result := False;
    end;
  end;
end;

function TryLoadRADStudioProject(const AFileName: string; out AProject: TRADStudioProject): Boolean;
var
  LPlatform: TProjectPlatform;
  LXMLNodeList: Variant;
  LXMLDocument: Variant;
  LText: string;
  I: Integer;
begin
  Result := FileExists(AFileName);
  if Result then
  begin
    AProject.FileName := AFileName;

    LXMLDocument := CreateOleObject('Msxml2.DOMDocument');
    try
      LXMLDocument.Async := False;
      LXMLDocument.ResolveExternals := False;
      LXMLDocument.Load(AFileName);
      if LXMLDocument.ParseError.ErrorCode <> 0 then
      begin
        TryShowErrorFmt(CustomMessage('RADStudioProjectErrorParsingProject'), [AFileName, IntToStr(LXMLDocument.ParseError.ErrorCode), IntToStr(LXMLDocument.ParseError.Line), IntToStr(LXMLDocument.ParseError.LinePos), LXMLDocument.ParseError.Reason]);
        Result := False;
        Exit;
      end;

      // IsPackage
      AProject.IsPackage := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup/AppType', '') = 'Package';

      // ProjectVersion
      AProject.ProjectVersion := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup/ProjectVersion', '');

      // IsInstallable
      AProject.IsInstallable := not StrToBoolDef(ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/RuntimeOnlyPackage', ''), False);

      // IsDesignOnly
      AProject.IsDesignOnly := StrToBoolDef(ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DesignOnlyPackage', ''), False);

      // Personality
      LText := ReadXMLNodeText(LXMLDocument, '/Project/ProjectExtensions/Borland.Personality', '');
      if SameText(LText, 'Delphi.Personality.12') then
        AProject.Personality := ppDelphi
      else if SameText(LText, 'CPlusPlusBuilder.Personality.12') then
        AProject.Personality := ppCBuilder
      else
      begin
        TryShowErrorFmt(CustomMessage('RADStudioProjectErrorParsingProjectUnknownPersonality'), [AFileName, LText]);
        Result := False;
        Exit;
      end;

      // Platforms
      LXMLNodeList := LXMLDocument.SelectNodes('/Project/ProjectExtensions/BorlandProject/Platforms/Platform');
      AProject.Platforms := [];
      for I := 0 to LXMLNodeList.Length-1 do
      begin
        if StrToBoolDef(LXMLNodeList.Item[I].Text, False) and TryGetXMLNodeAttribute(LXMLNodeList.Item[I], 'Value', LText) and
          TryStrToProjectPlatform(LText, LPlatform) then
        begin
          Include(AProject.Platforms, LPlatform);
        end;
      end;

      // SourcePaths & RequiresDCPCount
      LXMLNodeList := LXMLDocument.SelectNodes('/Project/ItemGroup/DCCReference/@Include');
      AProject.SourcePaths := [];
      AProject.RequiresDCPCount := 0;
      for I := 0 to LXMLNodeList.Length-1 do
      begin
        if EndsWithText(LXMLNodeList.Item[I].Text, '.pas') then
        begin
          AProject.SourceFiles := AppendString(AProject.SourceFiles, RemoveBackslash(ExpandFileName(AddBackslash(ExtractFilePath(AFileName)) + LXMLNodeList.Item[I].Text)), False);
          AProject.SourcePaths := AppendString(AProject.SourcePaths, RemoveBackslash(ExpandFileName(AddBackslash(ExtractFilePath(AFileName)) + ExtractFilePath(LXMLNodeList.Item[I].Text))), False);
        end
        else if EndsWithText(LXMLNodeList.Item[I].Text, '.dcp') then
          AProject.RequiresDCPCount := AProject.RequiresDCPCount + 1;
      end;

      AProject.BplOutputPath := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DCC_BplOutput', '');
      AProject.DcpOutputPath := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DCC_DcpOutput', '');
      AProject.DCUOutputPath := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DCC_DcuOutput', '');
      AProject.Description := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DCC_Description', '');
      AProject.DllSuffix := ReadXMLNodeText(LXMLDocument, '/Project/PropertyGroup[@Condition="''$(Base)''!=''''"]/DllSuffix', '');
    except
      TryShowErrorFmt(CustomMessage('RADStudioProjectErrorUnexpectedParsingProject'), [AFileName, GetExceptionMessage]);
      Result := False;
    end;
  end;
end;

function TryStrToProjectConfig(const AConfigName: string; out AConfig: TProjectConfig): Boolean;
begin
  Result := True;
  if SameText(AConfigName, 'Debug') then
    AConfig := pcDebug
  else if SameText(AConfigName, 'Release') then
    AConfig := pcRelease
  else
    Result := False;
end;

function TryStrToProjectConfigs(const AText: string; out AConfigs: TProjectConfigs): Boolean;
var
  LStrings: TArrayOfString;
  LConfig: TProjectConfig;
  I: Integer;
begin
  AConfigs := [];
  LStrings := SplitString(AText, ';');
  for I := 0 to GetArrayLength(LStrings)-1 do
  begin
    if not TryStrToProjectConfig(LStrings[I], LConfig) then
    begin
      Result := False;
      Exit;
    end;
    Include(AConfigs, LConfig);
  end;
  Result := AConfigs <> [];
end;

function TryStrToProjectPlatform(const APlatformName: string; out APlatform: TProjectPlatform): Boolean;
begin
  Result := True;
  if SameText(APlatformName, 'Win32') then
    APlatform := pfWin32
  else if SameText(APlatformName, 'Win64') then
    APlatform := pfWin64
  else if SameText(APlatformName, 'Android') then
    APlatform := pfAndroid
  else if SameText(APlatformName, 'Android32') then
    APlatform := pfAndroid
  else if SameText(APlatformName, 'Android64') then
    APlatform := pfAndroid64
  else if SameText(APlatformName, 'iOSDevice') then
    APlatform := pfiOSDevice32
  else if SameText(APlatformName, 'iOSDevice32') then
    APlatform := pfiOSDevice32
  else if SameText(APlatformName, 'iOSDevice64') then
    APlatform := pfiOSDevice64
  else if SameText(APlatformName, 'iOSSimARM64') then
    APlatform := pfiOSSimARM64
  else if SameText(APlatformName, 'iOSSimulator') then
    APlatform := pfiOSSimulator
  else if SameText(APlatformName, 'OSX32') then
    APlatform := pfOSX32
  else if SameText(APlatformName, 'OSX64') then
    APlatform := pfOSX64
  else if SameText(APlatformName, 'OSXARM64') then
    APlatform := pfOSXARM64
  else if SameText(APlatformName, 'Linux64') then
    APlatform := pfLinux64
  else
    Result := False;
end;

function TryStrToProjectPlatforms(const AText: string; out APlatforms: TProjectPlatforms): Boolean;
var
  LStrings: TArrayOfString;
  LPlatform: TProjectPlatform;
  I: Integer;
begin
  APlatforms := [];
  LStrings := SplitString(AText, ';');
  for I := 0 to GetArrayLength(LStrings)-1 do
  begin
    if not TryStrToProjectPlatform(LStrings[I], LPlatform) then
    begin
      Result := False;
      Exit;
    end;
    Include(APlatforms, LPlatform);
  end;
  Result := APlatforms <> [];
end;

// end.
#endif
