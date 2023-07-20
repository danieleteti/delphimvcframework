// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Filters.Redirect;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons;

type
  TMVCRedirectProtocolFilter = class(TCustomProtocolFilter)
  private
    fRedirectToURL: string;
    fRequestedPathInfos: TArray<String>;
  protected
    procedure DoFilter(Context: TWebContext); override;
  public
    constructor Create(const RequestedPathInfos: TArray<string>; const RedirectToURL: string);
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

constructor TMVCRedirectProtocolFilter.Create(const RequestedPathInfos: TArray<string>; const RedirectToURL: string);
begin
  inherited Create;
  fRequestedPathInfos := RequestedPathInfos;
  fRedirectToURL := RedirectToURL;
end;

procedure TMVCRedirectProtocolFilter.DoFilter(Context: TWebContext);
var
  I: Integer;
  lPathInfo: string;
  lHandled: Boolean;
begin
  lHandled := False;
  lPathInfo := Context.Request.PathInfo;
  for I := 0 to Pred(Length(fRequestedPathInfos)) do
  begin
    if lPathInfo = fRequestedPathInfos[I] then
    begin
      Context.Response.RawWebResponse.SendRedirect(fRedirectToURL);
      LogI(Format('Redirected from [%s] to [%s]', [lPathInfo, fRedirectToURL]));
      lHandled := True;
    end;
  end;

  if not lHandled then
  begin
    DoNext(Context);
  end;
end;

end.
