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
// *************************************************************************** }

unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.Generics.Collections;

type
  [MVCNameCase(ncCamelCase)]
  TCustomer = class
  private
    fID: Integer;
    fName: string;
    fCity: string;
  public
    property ID: Integer read fID write fID;
    property Name: string read fName write fName;
    property City: string read fCity write fCity;
  end;

  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function Index: String;

    [MVCPath('/reversedstrings/($Value)')]
    [MVCHTTPMethod([httpGET])]
    function GetReversedString(const Value: String): String;

    // In-memory "Customer" CRUD. Its only purpose here is to give the Trace
    // middleware real request/response bodies to log, so the trace output is
    // worth looking at. The store is a process-wide list guarded by a lock.
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    function GetCustomers: IMVCResponse;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpGET])]
    function GetCustomer(const ID: Integer): IMVCResponse;

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    function CreateCustomer(const [MVCFromBody] Customer: TCustomer): IMVCResponse;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpPUT])]
    function UpdateCustomer(const ID: Integer; const [MVCFromBody] Customer: TCustomer): IMVCResponse;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpDELETE])]
    function DeleteCustomer(const ID: Integer): IMVCResponse;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

var
  gCustomers: TObjectList<TCustomer>;
  gLock: TObject;
  gNextID: Integer;

function CloneCustomer(const ASrc: TCustomer): TCustomer;
begin
  Result := TCustomer.Create;
  Result.ID := ASrc.ID;
  Result.Name := ASrc.Name;
  Result.City := ASrc.City;
end;

// Caller must hold gLock. Returns nil when no customer has the given id.
function FindCustomer(const AID: Integer): TCustomer;
var
  lCustomer: TCustomer;
begin
  Result := nil;
  for lCustomer in gCustomers do
    if lCustomer.ID = AID then
      Exit(lCustomer);
end;

{ TMainController }

function TMainController.Index: String;
begin
  Result := 'Hello DelphiMVCFramework World';
end;

function TMainController.GetReversedString(const Value: String): String;
begin
  Result := System.StrUtils.ReverseString(Value.Trim);
end;

function TMainController.GetCustomers: IMVCResponse;
var
  lResult: TObjectList<TCustomer>;
  lCustomer: TCustomer;
begin
  lResult := TObjectList<TCustomer>.Create(True);
  TMonitor.Enter(gLock);
  try
    for lCustomer in gCustomers do
      lResult.Add(CloneCustomer(lCustomer));
  finally
    TMonitor.Exit(gLock);
  end;
  // OKResponse takes ownership of the list and frees it after serialization
  Result := OKResponse(lResult);
end;

function TMainController.GetCustomer(const ID: Integer): IMVCResponse;
var
  lFound: TCustomer;
begin
  TMonitor.Enter(gLock);
  try
    lFound := FindCustomer(ID);
    if lFound = nil then
      raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Customer %d not found', [ID]);
    Result := OKResponse(CloneCustomer(lFound));
  finally
    TMonitor.Exit(gLock);
  end;
end;

function TMainController.CreateCustomer(const [MVCFromBody] Customer: TCustomer): IMVCResponse;
var
  lNew: TCustomer;
begin
  TMonitor.Enter(gLock);
  try
    Inc(gNextID);
    lNew := CloneCustomer(Customer);
    lNew.ID := gNextID;
    gCustomers.Add(lNew);
    Result := CreatedResponse('/api/customers/' + lNew.ID.ToString, CloneCustomer(lNew));
  finally
    TMonitor.Exit(gLock);
  end;
end;

function TMainController.UpdateCustomer(const ID: Integer; const [MVCFromBody] Customer: TCustomer): IMVCResponse;
var
  lFound: TCustomer;
begin
  TMonitor.Enter(gLock);
  try
    lFound := FindCustomer(ID);
    if lFound = nil then
      raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Customer %d not found', [ID]);
    lFound.Name := Customer.Name;
    lFound.City := Customer.City;
    Result := OKResponse(CloneCustomer(lFound));
  finally
    TMonitor.Exit(gLock);
  end;
end;

function TMainController.DeleteCustomer(const ID: Integer): IMVCResponse;
var
  lFound: TCustomer;
begin
  TMonitor.Enter(gLock);
  try
    lFound := FindCustomer(ID);
    if lFound = nil then
      raise EMVCException.CreateFmt(HTTP_STATUS.NotFound, 'Customer %d not found', [ID]);
    gCustomers.Remove(lFound);
  finally
    TMonitor.Exit(gLock);
  end;
  Result := NoContentResponse;
end;

procedure SeedCustomers;
var
  lCustomer: TCustomer;
begin
  Inc(gNextID);
  lCustomer := TCustomer.Create;
  lCustomer.ID := gNextID;
  lCustomer.Name := 'Daniele Teti';
  lCustomer.City := 'Rome';
  gCustomers.Add(lCustomer);

  Inc(gNextID);
  lCustomer := TCustomer.Create;
  lCustomer.ID := gNextID;
  lCustomer.Name := 'John Doe';
  lCustomer.City := 'New York';
  gCustomers.Add(lCustomer);
end;

initialization

gLock := TObject.Create;
gCustomers := TObjectList<TCustomer>.Create(True);
SeedCustomers;

finalization

gCustomers.Free;
gLock.Free;

end.
