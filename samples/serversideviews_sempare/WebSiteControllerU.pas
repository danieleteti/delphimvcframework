// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
//
// ***************************************************************************
//
// Sempare Template Engine
//
// Copyright (c) 2019-2023 Conrad Vermeulen and Sempare Limited
//
// https://github.com/sempare/sempare-delphi-template-engine
//
// NOTE: The Sempare Template Engine is available under GPL or commercial license.
//
// Free as in speech, NOT Free as in beer.
//
// ***************************************************************************
//
// This example is licensed under the Apache License.
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
unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, JsonDataObjects, MVCFramework.Commons, DAL,
  System.Generics.Collections;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean); override;
    procedure GeneratePeopleListAsCSV;
  public
    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure PeopleList;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_CSV)]
    // RESTful API, requires ACCEPT=text/csv
    procedure ExportPeopleListAsCSV_API;

    [MVCPath('/people/formats/csv')]
    [MVCHTTPMethods([httpGET])]
    // Route usable by the browser, doesn't requires ACCEPT=text/csv
    procedure ExportPeopleListAsCSV;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure SavePerson(const [MVCFromBody] Person: TPerson);

    [MVCPath('/deleteperson')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure DeletePerson;

    [MVCPath('/new')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure NewPerson;

    [MVCPath('/edit/($guid)')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure EditPerson(guid: string);

    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure Index;

    [MVCPath('/showcase')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure SempareTemplateShowCase;
  end;

type
  TSelectedDevice = record
    name: string;
    selected: Boolean;
  end;

implementation

uses System.Rtti, System.SysUtils, Web.HTTPApp;

function GetSelectedDevices(const ADevices: TArray<string>; const AUserDevices: TArray<string>): TArray<TSelectedDevice>;
var
  i: integer;
  LDevice: string;
  LDevices: TArray<string>;
begin
  LDevices := ADevices;
  setlength(result, length(ADevices));
  TArray.Sort<string>(LDevices);
  for i := Low(LDevices) to High(LDevices) do
  begin
    result[i].name := ADevices[i];
    result[i].selected := false;
  end;
  for LDevice in AUserDevices do
  begin
    if TArray.BinarySearch<String>(LDevices, LDevice, i) then
      result[i].selected := true;
  end;
end;

function GetSelectedDevicesValue(const ADevices: TArray<string>; const AUserDevices: TArray<string>): TValue;
begin
  exit(TValue.From < TArray < TSelectedDevice >> (GetSelectedDevices(ADevices, AUserDevices)));
end;

{ TWebSiteController }

procedure TWebSiteController.DeletePerson;
var
  lGUID: string;
  LDAL: IPeopleDAL;
begin
  lGUID := Context.Request.Params['guid'];
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(lGUID);
  Redirect('/people');
end;

procedure TWebSiteController.EditPerson(guid: string);
var
  LDAL: IPeopleDAL;
  lPerson: TPerson;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPerson := LDAL.GetPersonByGUID(guid);
  try
    ViewData['person'] := lPerson;
    ViewData['devices'] := GetSelectedDevicesValue(LDAL.GetDevicesList, lPerson.Devices);
    LoadView(['editperson']);
    RenderResponseStream;
  finally
    lPerson.Free;
  end;
end;

procedure TWebSiteController.ExportPeopleListAsCSV;
begin
  GeneratePeopleListAsCSV;
  // define the correct behaviour to download the csv inside the browser
  ContentType := TMVCMediaType.TEXT_CSV;
  Context.Response.CustomHeaders.Values['Content-Disposition'] := 'attachment; filename=people.csv';
end;

procedure TWebSiteController.ExportPeopleListAsCSV_API;
begin
  GeneratePeopleListAsCSV;
end;

procedure TWebSiteController.GeneratePeopleListAsCSV;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    LoadView(['people_list.csv']);
    RenderResponseStream;
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

procedure TWebSiteController.SempareTemplateShowCase;
var
  LDAL: IPeopleDAL;
  lPeople, lPeople2: TPeople;
  lMyObj: TMyObj;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := nil;
  lPeople2 := nil;
  lMyObj := nil;
  try
    lPeople := LDAL.GetPeople;
    lPeople2 := TObjectList<TPerson>.Create;
    lMyObj := TMyObj.Create;
    lMyObj.RawHTML := '<h1>This is</h1>Raw<br><span>HTML</span>';
    ViewData['people'] := lPeople;
    ViewData['people2'] := lPeople2;
    ViewData['myobj'] := lMyObj;
    LoadView(['showcase']);
    RenderResponseStream;
  finally
    lMyObj.Free;
    lPeople2.Free;
    lPeople.Free;
  end;
end;

procedure TWebSiteController.NewPerson;
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  ViewData['devices'] := GetSelectedDevicesValue(LDAL.GetDevicesList, nil);
  LoadView(['editperson']);
  RenderResponseStream;
end;

procedure TWebSiteController.OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  ContentType := 'text/html';
  Handled := false;
end;

procedure TWebSiteController.PeopleList;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    LoadView(['people_list']);
    RenderResponseStream;
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.SavePerson(const [MVCFromBody] Person: TPerson);
var
  LPeopleDAL: IPeopleDAL;
begin
  if Person.FirstName.IsEmpty or Person.LastName.IsEmpty or (Person.Age <= 0) then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data', 'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(Person.FirstName, Person.LastName, Person.Age, Person.Devices);
  Redirect('/people');
end;

end.
