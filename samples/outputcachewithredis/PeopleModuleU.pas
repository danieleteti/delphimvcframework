unit PeopleModuleU;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, PersonBO, System.Generics.Collections, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.IBBase, FireDAC.Phys.IB;

type
  TPeopleModule = class(TDataModule)
    qryPeople: TFDQuery;
    updPeople: TFDUpdateSQL;
    Conn: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    procedure ConnBeforeConnect(Sender: TObject);
  public
    procedure CreatePerson(APerson: TPerson);
    procedure DeletePerson(AID: Integer);
    procedure UpdatePerson(APerson: TPerson);
    function GetPersonByID(AID: Integer): TPerson;
    function FindPeople(ASearchText: String; APage: Integer): TObjectList<TPerson>;
    function GetPeople: TObjectList<TPerson>;
  end;

implementation

uses
  MVCFramework.Serializer.Commons;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

{ TPeopleModule }

procedure TPeopleModule.ConnBeforeConnect(Sender: TObject);
begin
  inherited;
  Conn.Params.Values['Database'] := '..\..\..\..\DATA\SAMPLES.IB';
end;

procedure TPeopleModule.CreatePerson(APerson: TPerson);
var
  InsCommand: TFDCustomCommand;
begin
  InsCommand := updPeople.Commands[arInsert];
  Mapper.ObjectToFDParameters(InsCommand.Params, APerson, 'NEW_');
  InsCommand.Execute;
  APerson.ID := Conn.GetLastAutoGenValue('gen_people_id');
end;

procedure TPeopleModule.DeletePerson(AID: Integer);
var
  DelCommand: TFDCustomCommand;
begin
  DelCommand := updPeople.Commands[arDelete];
  DelCommand.ParamByName('OLD_ID').AsInteger := AID;
  DelCommand.Execute;
end;

function TPeopleModule.FindPeople(ASearchText: String; APage: Integer): TObjectList<TPerson>;
var
  StartRec, EndRec: Integer;
begin
  Dec(APage); // page 0 => 0, 9, page 1 => 10, 19, page 3 => 20, 29
  StartRec := (10 * APage);
  EndRec := StartRec + 10 - 1;
  qryPeople.Open('SELECT * FROM PEOPLE WHERE ' +
    'FIRST_NAME CONTAINING :SEARCH_TEXT_1 OR ' +
    'LAST_NAME CONTAINING :SEARCH_TEXT_2 OR ' +
    'EMAIL CONTAINING :SEARCH_TEXT_3 ' +
    'ORDER BY LAST_NAME, FIRST_NAME ' +
    Format('ROWS %d TO %d', [StartRec, EndRec]),
    [ASearchText, ASearchText, ASearchText]);
  Result := qryPeople.AsObjectList<TPerson>;
end;

function TPeopleModule.GetPersonByID(AID: Integer): TPerson;
begin
  qryPeople.Open('SELECT * FROM PEOPLE WHERE ID = :ID', [AID]);
  Result := qryPeople.AsObject<TPerson>;
end;

function TPeopleModule.GetPeople: TObjectList<TPerson>;
begin
  qryPeople.Open;
  Result := qryPeople.AsObjectList<TPerson>;
end;

procedure TPeopleModule.UpdatePerson(APerson: TPerson);
var
  UpdCommand: TFDCustomCommand;
begin
  UpdCommand := updPeople.Commands[arUpdate];
  Mapper.ObjectToFDParameters(
    UpdCommand.Params,
    APerson, 'NEW_');
  UpdCommand.ParamByName('OLD_ID').AsInteger := APerson.ID;
  UpdCommand.Execute;
end;

end.
