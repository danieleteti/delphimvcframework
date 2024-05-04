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
unit DAL;

interface

uses
  System.JSON,
  MVCFramework.SystemJSONUtils,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons;

type

  [MVCNameCase(ncLowerCase)]
  TPerson = class
  private
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
    FDevices: TArray<string>;
    FGUID: string;
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetAge(const Value: Integer);
    procedure SetGUID(const Value: string);
    procedure SetDevices(const Value: TArray<string>);
  public
    [MVCNameAs('first_name')]
    property FirstName: string read FFirstName write SetFirstName;
    [MVCNameAs('last_name')]
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property Devices: TArray<string> read FDevices write SetDevices;
    property GUID: string read FGUID write SetGUID;
  end;

{$M+}

  TMyObj = class
  private
    FRawHTML: String;
    procedure SetRawHTML(const Value: String);
  published
    property RawHTML: String read FRawHTML write SetRawHTML;
  end;
{$M-}

  TDevice = class
  private
    fDeviceName: string;
    fSelected: Boolean;
  public
    property DeviceName: string read fDeviceName write fDeviceName;
    property Selected: Boolean read fSelected write fSelected;
    constructor Create(aDeviceName: string; aSelected: Boolean);
  end;

  TDeviceList = class(TObjectList<TDevice>)
  public
    function Contains(const aDeviceName: string): Boolean;
    function IndexOf(const aDeviceName: string): Integer;
  end;

  TPeople = TObjectList<TPerson>;

  IPeopleDAL = interface
    ['{3E534A3E-EAEB-44ED-B74E-EFBBAAAE11B4}']
    function GetPeople: TPeople;
    procedure AddPerson(FirstName, LastName: string; Age: Integer; Items: TArray<string>);
    procedure DeleteByGUID(GUID: string);
    function GetPersonByGUID(GUID: string): TPerson;
    function GetDevicesList: TArray<String>;
  end;

  TPeopleDAL = class(TInterfacedObject, IPeopleDAL)
  private const
    DATAFILE: string = 'people.data';
  public
    function GetPeople: TPeople;
    procedure AddPerson(FirstName, LastName: string; Age: Integer; Items: TArray<string>);
    procedure DeleteByGUID(GUID: string);
    function GetPersonByGUID(GUID: string): TPerson;
    function GetDevicesList: TArray<String>;
  end;

  TServicesFactory = class sealed
    class function GetPeopleDAL: IPeopleDAL;
  end;

implementation

uses
  System.SyncObjs,
  System.IOUtils,
  MVCFramework.Serializer.Defaults,
  System.SysUtils;

var
  // Hey! The storage is a simple json file, so some synchronization is needed
  _CS: TCriticalSection = nil;

  { TSimpleDAL }

procedure TPeopleDAL.AddPerson(FirstName, LastName: string; Age: Integer; Items: TArray<string>);
var
  lPeople: TPeople;
  lPerson: TPerson;
begin
  _CS.Enter;
  try
    lPeople := GetPeople;
    try
      lPerson := TPerson.Create;
      lPeople.Add(lPerson);
      lPerson.FirstName := FirstName;
      lPerson.LastName := LastName;
      lPerson.Age := Age;
      lPerson.Devices := Items;
      lPerson.GUID := TGuid.NewGuid.ToString.Replace('{', '').Replace('}', '').Replace('-', '');
      TFile.WriteAllText(DATAFILE, GetDefaultSerializer.SerializeCollection(lPeople));
    finally
      lPeople.Free;
    end;
  finally
    _CS.Leave;
  end;
end;

class function TServicesFactory.GetPeopleDAL: IPeopleDAL;
begin
  Result := TPeopleDAL.Create;
end;

procedure TPeopleDAL.DeleteByGUID(GUID: string);
var
  LJPeople: TPeople;
  I: Integer;
begin
  _CS.Enter;
  try
    LJPeople := GetPeople;
    try
      for I := 0 to LJPeople.Count - 1 do
      begin
        if LJPeople[I].GUID = GUID then
        begin
          LJPeople.Delete(I);
          break;
        end;
      end;
      TFile.WriteAllText(DATAFILE, GetDefaultSerializer.SerializeCollection(LJPeople));
    finally
      LJPeople.Free;
    end;
  finally
    _CS.Leave;
  end;
end;

function TPeopleDAL.GetDevicesList: TArray<String>;
begin
  Result := ['smartphone', 'dumbphone', 'laptop', 'desktop'];
end;

function TPeopleDAL.GetPeople: TPeople;
var
  LData: string;
begin
  _CS.Enter;
  try
    Result := TPeople.Create;
    if TFile.Exists(DATAFILE) then
      LData := TFile.ReadAllText(DATAFILE).Trim;
    if not LData.IsEmpty then
    begin
      GetDefaultSerializer.DeserializeCollection(LData, Result, TPerson);
    end;
  finally
    _CS.Leave;
  end;
end;

function TPeopleDAL.GetPersonByGUID(GUID: string): TPerson;
var
  lPeople: TPeople;
  lPerson: TPerson;
begin
  Result := nil;
  lPeople := GetPeople;
  try
    for lPerson in lPeople do
    begin
      if lPerson.GUID = GUID then
      begin
        Result := lPeople.Extract(lPerson);
        break;
      end;
    end;
  finally
    lPeople.Free;
  end;
end;

{ TPerson }

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetDevices(const Value: TArray<string>);
begin
  FDevices := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetGUID(const Value: string);
begin
  FGUID := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TDevice }

constructor TDevice.Create(aDeviceName: string; aSelected: Boolean);
begin
  inherited Create;
  fDeviceName := aDeviceName;
  fSelected := aSelected;
end;

{ TDeviceList }

function TDeviceList.Contains(const aDeviceName: string): Boolean;
begin
  Result := IndexOf(aDeviceName) > -1;
end;

function TDeviceList.IndexOf(const aDeviceName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Self.Count - 1 do
  begin
    if SameText(Self[I].DeviceName, aDeviceName) then
      Exit(I);
  end;
end;

{ TRawObj }

procedure TMyObj.SetRawHTML(const Value: String);
begin
  FRawHTML := Value;
end;

initialization

_CS := TCriticalSection.Create;

finalization

_CS.Free;

end.
