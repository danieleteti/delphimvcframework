unit TestPeopleModuleU;

interface

uses
  DUnitX.TestFramework, System.Generics.Collections, FireDAC.Stan.Error, Data.DB,
  PeopleModuleU, FireDAC.Stan.Intf, FireDAC.DApt, FireDAC.Stan.Pool,
  FireDAC.Comp.Client, FireDAC.Stan.Async, FireDAC.DatS, FireDAC.UI.Intf,
  FireDAC.Stan.Param, FireDAC.Phys.IB, FireDAC.Phys, PersonBO, FireDAC.Stan.Option,
  System.Classes, System.SysUtils, FireDAC.Phys.IBBase, FireDAC.Stan.Def,
  FireDAC.DApt.Intf, FireDAC.Phys.FB, FireDAC.Comp.DataSet, FireDAC.Phys.Intf;

type
  // Test methods for class TPeopleModule

  TestTPeopleModule = class(TTestCase)
  strict private
    FPeopleModule: TPeopleModule;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnBeforeConnect;
    procedure TestCreatePerson;
    procedure TestDeletePerson;
    procedure TestUpdatePerson;
    procedure TestGetPersonByID;
    procedure TestFindPeople;
    procedure TestGetPeople;
  end;

implementation

procedure TestTPeopleModule.SetUp;
begin
  FPeopleModule := TPeopleModule.Create(nil);
end;

procedure TestTPeopleModule.TearDown;
begin
  FPeopleModule.Free;
  FPeopleModule := nil;
end;

procedure TestTPeopleModule.TestConnBeforeConnect;
var
  Sender: TObject;
begin
  // TODO: Setup method call parameters
  FPeopleModule.ConnBeforeConnect(Sender);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestCreatePerson;
var
  APerson: TPerson;
begin
  // TODO: Setup method call parameters
  FPeopleModule.CreatePerson(APerson);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestDeletePerson;
var
  AID: Integer;
begin
  // TODO: Setup method call parameters
  FPeopleModule.DeletePerson(AID);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestUpdatePerson;
var
  APerson: TPerson;
begin
  // TODO: Setup method call parameters
  FPeopleModule.UpdatePerson(APerson);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestGetPersonByID;
var
  ReturnValue: TPerson;
  AID: Integer;
begin
  // TODO: Setup method call parameters
  ReturnValue := FPeopleModule.GetPersonByID(AID);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestFindPeople;
var
  ReturnValue: TObjectList<TPerson>;
  APage: Integer;
  ASearchText: string;
begin
  // TODO: Setup method call parameters
  ReturnValue := FPeopleModule.FindPeople(ASearchText, APage);
  // TODO: Validate method results
end;

procedure TestTPeopleModule.TestGetPeople;
var
  ReturnValue: TObjectList<TPerson>;
begin
  ReturnValue := FPeopleModule.GetPeople(1);
  // TODO: Validate method results
end;

initialization

// Register any test cases with the test runner
//TDUnitX.RegisterTestFixture(TestTPeopleModule);

end.
