// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit ControllersU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  [MVCPath('/api')]
  TSampleController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]
    function HelloWorld: IMVCResponse;

    [MVCPath('/echo')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    function EchoBody: IMVCResponse;

    [MVCPath('/info')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function ServerInfo: IMVCResponse;

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function GetCustomers: IMVCResponse;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections, JsonDataObjects;

function TSampleController.HelloWorld: IMVCResponse;
begin
  Result := OKResponse('Hello World from DelphiMVCFramework!');
end;

function TSampleController.EchoBody: IMVCResponse;
begin
  Result := OKResponse(Context.Request.Body);
end;

function TSampleController.ServerInfo: IMVCResponse;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := TJDOJsonObject.Create;
  lJSON.S['framework'] := 'DelphiMVCFramework';
  lJSON.S['version'] := DMVCFRAMEWORK_VERSION;
  Result := OKResponse(lJSON);
end;

function TSampleController.GetCustomers: IMVCResponse;
var
  lJSON: TJDOJsonArray;
  lObj: TJDOJsonObject;
begin
  lJSON := TJDOJsonArray.Create;

  lObj := lJSON.AddObject;
  lObj.S['id'] := '1';
  lObj.S['name'] := 'Daniele Teti';
  lObj.S['city'] := 'Rome';

  lObj := lJSON.AddObject;
  lObj.S['id'] := '2';
  lObj.S['name'] := 'Peter Parker';
  lObj.S['city'] := 'New York';

  lObj := lJSON.AddObject;
  lObj.S['id'] := '3';
  lObj.S['name'] := 'Bruce Wayne';
  lObj.S['city'] := 'Gotham City';

  Result := OKResponse(lJSON);
end;

end.
