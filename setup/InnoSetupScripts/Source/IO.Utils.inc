#ifndef UnitIOUtils
#define UnitIOUtils

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
// unit IO.Utils;

// interface

type
  TSearchOption = (soTopDirectoryOnly, soAllDirectories);

/// <summary> Combine two paths (including combination of directory and filename) </summary>
function CombinePath(const APath, ASubPath: string): string; forward;
/// <summary> Copy directory folder </summary>
function CopyDirectory(const AOriginPath, ADestPath: string; const AOverwriteExisting: Boolean): Boolean; forward;
/// <summary> Copy file to a directory in a "force" mode (creating the path if not exists and overwrite existing) </summary>
function CopyFileToDir(const AFileName, ADestPath: string): Boolean; forward;
/// <summary> Get files of a path with an optional filename. The filename accepts the wildcard *, like "*.json" </summary>
function GetFiles(APath, AFileName: string; const ASearchOption: TSearchOption): TArrayOfString; forward;
/// <summary> Replace all matches of a folder name in a path </summary>
function ReplaceFolderNameInPath(const APath, AOldFolderName, ANewFolderName: string; const ACaseSensitive: Boolean): string; forward;
/// <summary> Replace all matches of a string in a file and return the success in load and save the file (even without changes) </summary>
function ReplaceStringInFile(const AFileName, AOldValue, ANewValue: string; const ACaseSensitive: Boolean): Boolean; forward;

// implementation

// uses
  #include ".\String.Utils.inc"

function CombinePath(const APath, ASubPath: string): string;
begin
  Result := ASubPath;
  if StartsWithText(Result, '\') then
    Result := Copy(Result, 2, Length(Result) - 1);
  Result := AddBackslash(APath) + Result;
end;

function CopyDirectory(const AOriginPath, ADestPath: string; const AOverwriteExisting: Boolean): Boolean;
var
  LOriginFilePath: string;
  LDestFilePath: string;
  LDestFileName: string;
  LFiles: TArrayOfString;
  I: Integer;
begin
  Result := DirExists(ADestPath) or ForceDirectories(ADestPath);
  if Result then
  begin
    LFiles := GetFiles(AOriginPath, '*', soAllDirectories);
    for I := 0 to GetArrayLength(LFiles) - 1 do
    begin
      LOriginFilePath := ExtractFilePath(LFiles[I]);
      LDestFilePath := CombinePath(ADestPath, Copy(LOriginFilePath, Length(AOriginPath) + 1, Length(LOriginFilePath) - (Length(AOriginPath) + 1)));
      if not DirExists(LDestFilePath) then
      begin
        Result := ForceDirectories(LDestFilePath);
        if not Result then
        begin
          Log(Format('IO.Utils.CopyDirectory: Failed to create the dir "%s"', [LDestFilePath]));
          Exit;
        end;
      end;
      LDestFileName := CombinePath(LDestFilePath, ExtractFileName(LFiles[I]));
      if (not AOverwriteExisting) and FileExists(LDestFileName) then
        Continue;
      Result := FileCopy(LFiles[I], LDestFileName, not AOverwriteExisting);
      if not Result then
      begin
        Log(Format('IO.Utils.CopyDirectory: Failed to copy the file "%s" to "%s".', [LFiles[I], LDestFileName]));
        Exit;
      end;
    end;
  end
  else
    Log(Format('IO.Utils.CopyDirectory: Failed to create the directory "%s".', [ADestPath]));
end;

function CopyFileToDir(const AFileName, ADestPath: string): Boolean;
begin
  Result := FileExists(AFileName) and (DirExists(ADestPath) or ForceDirectories(ADestPath)) and
    FileCopy(AFileName, CombinePath(ADestPath, ExtractFileName(AFileName)), False);
end;

function GetFiles(APath, AFileName: string; const ASearchOption: TSearchOption): TArrayOfString;
var
  LFindRec: TFindRec;
  LSubDirResult: TArrayOfString;
  I: Integer;
begin
  Result := [];
  APath := AddBackslash(APath);

  if FindFirst(APath + AFileName, LFindRec) then
  begin
    try
      repeat
        if (LFindRec.Name <> '.') and (LFindRec.Name <> '..') and
          ((LFindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0) and FileExists(APath + LFindRec.Name) then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := APath + LFindRec.Name;
        end;
      until not FindNext(LFindRec);
    finally
      FindClose(LFindRec);
    end;
  end;

  if (ASearchOption = soAllDirectories) and FindFirst(APath + '*', LFindRec) then
  begin
    try
      repeat
        if (LFindRec.Name <> '.') and (LFindRec.Name <> '..') and
          DirExists(AddBackslash(APath + LFindRec.Name)) then
        begin
          LSubDirResult := GetFiles(AddBackslash(APath + LFindRec.Name), AFileName, ASearchOption);
          for I := 0 to GetArrayLength(LSubDirResult) -1 do
          begin
            SetArrayLength(Result, GetArrayLength(Result) + 1);
            Result[GetArrayLength(Result) - 1] := LSubDirResult[I];
          end;
        end;
      until not FindNext(LFindRec);
    finally
      FindClose(LFindRec);
    end;
  end;
end;

function ReplaceFolderNameInPath(const APath, AOldFolderName, ANewFolderName: string; const ACaseSensitive: Boolean): string;
begin
  Result := JoinStrings(ReplaceString(SplitString(APath, '\'), AOldFolderName, ANewFolderName, ACaseSensitive, True), '\', False);
end;

function ReplaceStringInFile(const AFileName, AOldValue, ANewValue: string; const ACaseSensitive: Boolean): Boolean;
var
  LLines: TArrayOfString;
begin
  Result := LoadStringsFromFile(AFileName, LLines) and SaveStringsToUTF8File(AFileName, ReplaceString(LLines, AOldValue, ANewValue, ACaseSensitive, False), False);
  if not Result then
    Log(Format('IO.Utils.ReplaceStringInFile: Failed to load or save strings in file "%s".', [AFileName]));
end;

// end.
#endif
