{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Json.Common.Helpers;

interface

uses
  System.JSON;

type
  TJSONAncestorHelper = class helper for TJSONAncestor
  public
    function Format: string;
  end;

  TJsonObjectHelper = class helper for TJsonObject
  public
    procedure AddPair(const pName: string; const pValue: Extended); overload;
  end;

implementation

{ TJSONAncestorHelper }

function TJSONAncestorHelper.Format: string;
var
  vJsonString: string;
  vChar: Char;
  vEOL: string;
  vIndent: string;
  vLeftIndent: string;
  vIsEOL: Boolean;
  vIsInString: Boolean;
  vIsEscape: Boolean;
begin
  vEOL := #13#10;
  vIndent := '  ';
  vIsEOL := true;
  vIsInString := false;
  vIsEscape := false;
  vJsonString := Self.ToString;
  for vChar in vJsonString do
  begin
    if not vIsInString and ((vChar = '{') or (vChar = '[')) then
    begin
      if not vIsEOL then
        Result := Result + vEOL;
      Result := Result + vLeftIndent + vChar + vEOL;
      vLeftIndent := vLeftIndent + vIndent;
      Result := Result + vLeftIndent;
      vIsEOL := true;
    end
    else if not vIsInString and (vChar = ',') then
    begin
      vIsEOL := false;
      Result := Result + vChar + vEOL + vLeftIndent;
    end
    else if not vIsInString and ((vChar = '}') or (vChar = ']')) then
    begin
      Delete(vLeftIndent, 1, Length(vIndent));
      if not vIsEOL then
        Result := Result + vEOL;
      Result := Result + vLeftIndent + vChar + vEOL;
      vIsEOL := true;
    end
    else
    begin
      vIsEOL := false;
      Result := Result + vChar;
    end;
    vIsEscape := (vChar = '\') and not vIsEscape;
    if not vIsEscape and (vChar = '"') then
      vIsInString := not vIsInString;
  end;
end;

{ TJsonObjectHelper }

procedure TJsonObjectHelper.AddPair(const pName: string; const pValue: Extended);
begin
  Self.AddPair(pName, TJsonNumber.Create(pValue));
end;

end.
