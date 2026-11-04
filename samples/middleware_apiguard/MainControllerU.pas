// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// Contributor of this file: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it
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

unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCDoc('Plain endpoint: normal traffic never trips a rule')]
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    function Hello: IMVCResponse;

    [MVCDoc('Demo login: 401 on wrong credentials, which the "bruteforce" rule counts')]
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    function Login: IMVCResponse;
  end;

implementation

uses
  JsonDataObjects;

function TMainController.Hello: IMVCResponse;
begin
  Result := OKResponse('Hello from a guarded API');
end;

function TMainController.Login: IMVCResponse;
var
  lBody: TJsonObject;
  lValid: Boolean;
begin
  lBody := StrToJSONObject(Context.Request.Body); // nil when the body is not a JSON object
  try
    // demo credentials only: a real service checks a hashed password
    lValid := Assigned(lBody) and (lBody.S['username'] = 'admin') and (lBody.S['password'] = 'secret');
  finally
    lBody.Free;
  end;
  if lValid then
    Result := OKResponse('Logged in')
  else
    Result := UnauthorizedResponse('Invalid credentials');
end;

end.
