unit PeopleModuleU;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, PersonBO, System.Generics.Collections, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.IBBase, FireDAC.Phys.IB, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs;

type
  TPeopleModule = class(TDataModule)
    qryPeople: TFDQuery;
    updPeople: TFDUpdateSQL;
    Conn: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    procedure ConnBeforeConnect(Sender: TObject);
    procedure ConnAfterConnect(Sender: TObject);
  public
    procedure CreatePerson(APerson: TPerson);
    procedure DeletePerson(AID: Integer);
    procedure UpdatePerson(APerson: TPerson);
    function GetPersonByID(AID: Integer): TPerson;
    function GetPhotoByID(AID: Integer): TStream;
    function FindPeople(ASearchText: string; APage: Integer): TObjectList<TPerson>;
    function GetPeople(APage: Integer): TObjectList<TPerson>;
  end;

implementation

uses
  System.Math, CommonsU, System.IOUtils, MVCFramework.DataSet.Utils, MVCFramework.FireDAC.Utils;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

{ TPeopleModule }

procedure TPeopleModule.ConnAfterConnect(Sender: TObject);
begin
  // Conn.ExecSQL('DROP TABLE PEOPLE');
  Conn.ExecSQL
    ('CREATE TABLE IF NOT EXISTS PEOPLE (ID INTEGER PRIMARY KEY AUTOINCREMENT, FIRST_NAME, LAST_NAME, WORK_PHONE_NUMBER, MOBILE_PHONE_NUMBER, EMAIL) ');
end;

procedure TPeopleModule.ConnBeforeConnect(Sender: TObject);
begin
  inherited;
  Conn.Params.Values['Database'] := ChangeFileExt(GetModuleName(HInstance), 'db');
end;

procedure TPeopleModule.CreatePerson(APerson: TPerson);
var
  InsCommand: TFDCustomCommand;
begin
  InsCommand := updPeople.Commands[arInsert];
  TFireDACUtils.ObjectToParameters(InsCommand.Params, APerson, 'NEW_');
  InsCommand.Execute;
  APerson.ID := Conn.GetLastAutoGenValue('');
end;

procedure TPeopleModule.DeletePerson(AID: Integer);
var
  DelCommand: TFDCustomCommand;
begin
  DelCommand := updPeople.Commands[arDelete];
  DelCommand.ParamByName('OLD_ID').AsInteger := AID;
  DelCommand.Execute;
end;

function TPeopleModule.FindPeople(ASearchText: string; APage: Integer): TObjectList<TPerson>;
var
  StartRec, EndRec: Integer;
  lMacro: TFDMacro;
begin
  Sleep(2000); // just to mimic a slow database query
  GetLimitByPage(APage, StartRec, EndRec);
  qryPeople.Macros.Clear;
  lMacro := qryPeople.Macros.Add;
  lMacro.AsString := '%' + ASearchText + '%';
  lMacro.Name := 'searchtext';

  qryPeople.Open('SELECT * FROM PEOPLE WHERE ' +
    'FIRST_NAME LIKE &searchtext ' +
    'OR LAST_NAME LIKE &searchtext ' +
    'OR EMAIL LIKE &searchtext ' +
    'ORDER BY LAST_NAME, FIRST_NAME ' +
    Format('LIMIT %d,%d', [StartRec, EndRec]),
    []);

  Result := qryPeople.AsObjectList<TPerson>;
end;

function TPeopleModule.GetPersonByID(AID: Integer): TPerson;
begin
  qryPeople.Open('SELECT * FROM PEOPLE WHERE ID = :ID', [AID]);
  Result := qryPeople.AsObject<TPerson>;
end;

function TPeopleModule.GetPhotoByID(AID: Integer): TStream;
begin
  Result := TFileStream.Create(Format('photos\photo_%4.4d.png', [AID]),
    fmOpenRead or fmShareDenyWrite);
end;

function TPeopleModule.GetPeople(APage: Integer): TObjectList<TPerson>;
var
  StartRec, EndRec: Integer;
begin
  GetLimitByPage(APage, StartRec, EndRec);
  qryPeople.Open('SELECT * FROM PEOPLE ORDER BY LAST_NAME, FIRST_NAME ' +
    Format('ROWS %d to %d', [StartRec, EndRec]));
  Result := qryPeople.AsObjectList<TPerson>;
end;

procedure TPeopleModule.UpdatePerson(APerson: TPerson);
var
  UpdCommand: TFDCustomCommand;
begin
  UpdCommand := updPeople.Commands[arUpdate];
  TFireDACUtils.ObjectToParameters(
    UpdCommand.Params,
    APerson, 'NEW_');
  UpdCommand.ParamByName('OLD_ID').AsInteger := APerson.ID;
  UpdCommand.Execute;
end;

end.
