{***************************************************************************}
{                                                                           }
{           Delphi MVC Framework                                            }
{                                                                           }
{           Copyright (c) 2010-2015 DMVCFramework Team                      }
{                                                                           }
{           https://github.com/danieleteti/delphimvcframework               }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit uGlobalVars;

interface

uses
  Classes, SysUtils;

var
  gAppName, gAppPath, gAppExe: string;

implementation

uses
  ioutils;

initialization

gAppExe := ExtractFileName(GetModuleName(HInstance) { ParamStr(0) } );
gAppName := ChangeFileExt(gAppExe, '');
// if not IsConsole then
// gAppPath := IncludeTrailingPathDelimiter(TPath.GetPublicPath)
// else
gAppPath := IncludeTrailingPathDelimiter
  (ExtractFilePath(GetModuleName(HInstance) { ParamStr(0) } ));

finalization

end.
