// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit Person.Controller;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  Person;

type

  [MVCPath('/persons')]
  TPersonController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); override;
  public
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure FindOne(id: Int64);

    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure FindAll;

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure Insert;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure Update(id: Int64);

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure Delete(id: Int64);
  end;

implementation

uses
  MVCFramework.Logger;

procedure TPersonController.Delete(id: Int64);
begin

end;

procedure TPersonController.FindAll;
var
  Persons: TObjectList<TPerson>;
  Person: TPerson;
  I: Integer;
begin
  Persons := TObjectList<TPerson>.Create;
  try
    for I := 1 to 10 do
    begin
      Person := TPerson.Create;
      Person.Id := I;
      Person.Name := 'Name ' + IntToStr(I);
      Person.Address := 'Address ' + IntToStr(I);
      Person.Birthday := Date - I;
      Persons.Add(Person);
    end;
    Self.Render<TPerson>(Persons, False);
  finally
    Persons.Free;
  end;
end;

procedure TPersonController.FindOne(id: Int64);
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.Id := id;
    Person.Name := 'Name ' + IntToStr(id);
    Person.Address := 'Address ' + IntToStr(id);
    Person.Birthday := Date - id;
    Self.Render(Person, False);
  finally
    Person.Free;
  end;
end;

procedure TPersonController.Insert;
begin

end;

procedure TPersonController.Update(id: Int64);
begin

end;

procedure TPersonController.OnAfterAction(AContext: TWebContext; const AActionName: string);
begin
  inherited;
end;

procedure TPersonController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  inherited;
end;

end.
