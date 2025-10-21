// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MainFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  MVCFramework.Repository,
  EntitiesU,
  RepositoriesU, FireDAC.Phys.PGDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBDef, FireDAC.Phys.MSSQLDef, FireDAC.Phys.MySQLDef, FireDAC.Phys.MySQL, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL, FireDAC.Phys.IB, FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    btnBasicCRUD: TButton;
    btnQueryOperations: TButton;
    btnRQLOperations: TButton;
    btnNamedQueries: TButton;
    btnCustomMethods: TButton;
    Memo1: TMemo;
    btnAdvancedRepository: TButton;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Panel1: TPanel;
    btnRunAllShowcases: TButton;
    btnClearLog: TButton;
    procedure btnBasicCRUDClick(Sender: TObject);
    procedure btnQueryOperationsClick(Sender: TObject);
    procedure btnRQLOperationsClick(Sender: TObject);
    procedure btnNamedQueriesClick(Sender: TObject);
    procedure btnCustomMethodsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAdvancedRepositoryClick(Sender: TObject);
    procedure btnRunAllShowcasesClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    procedure Log(const Value: string);
    procedure LoadSampleData;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MVCFramework.ActiveRecord,
  MVCFramework.Logger,
  System.Generics.Collections,
  FDConnectionConfigU,
  EngineChoiceFormU,
  Data.DB,
  FireDAC.Comp.Client,
  AllShowcasesU;

const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');

procedure TMainForm.Log(const Value: string);
begin
  Memo1.Lines.Add(Value);
  Application.ProcessMessages;
end;

procedure TMainForm.LoadSampleData;
var
  lCustomer: TCustomer;
  I: Integer;
  lRepo: IMVCRepository<TCustomer>;
begin
  Log('** Loading Sample Data');

  // Delete existing data
  lRepo := TCustomerRepository.Create;
  lRepo.DeleteAll;

  // Create sample customers
  for I := 1 to 20 do
  begin
    lCustomer := TCustomer.Create;
    try
      lCustomer.Code := Format('C%3.3d', [I]);
      lCustomer.CompanyName := Stuff[Random(Length(Stuff))] + ' ' +
                                CompanySuffix[Random(Length(CompanySuffix))];
      lCustomer.City := Cities[Random(Length(Cities))];
      lCustomer.Rating := Random(5) + 1;
      lCustomer.Note := 'Sample customer ' + I.ToString;
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Loaded 20 sample customers');
end;

procedure TMainForm.btnBasicCRUDClick(Sender: TObject);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('** Basic CRUD Operations with Repository');

  lRepo := TCustomerRepository.Create;

  // Create
  lCustomer := TCustomer.Create;
  try
    lCustomer.Code := 'REPO001';
    lCustomer.CompanyName := 'Repository Test Corp.';
    lCustomer.City := 'Rome';
    lCustomer.Rating := 5;
    lCustomer.Note := 'Created via Repository';
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log('Created customer with ID: ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  // Read
  lCustomer := lRepo.GetByPK(lID);
  try
    Log('Retrieved: ' + lCustomer.ToString);

    // Update
    lCustomer.Note := 'Updated via Repository';
    lRepo.Update(lCustomer);
    Log('Updated customer ID: ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  // Read again to verify update
  lCustomer := lRepo.GetByPK(lID);
  try
    Log('After update: ' + lCustomer.ToString);
  finally
    lCustomer.Free;
  end;

  // Delete
  lCustomer := lRepo.GetByPK(lID);
  try
    lRepo.Delete(lCustomer);
    Log('Deleted customer ID: ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  // Verify deletion
  lCustomer := lRepo.GetByPK(lID, False);
  if lCustomer = nil then
    Log('Customer successfully deleted')
  else
  begin
    lCustomer.Free;
    Log('ERROR: Customer still exists!');
  end;
end;

procedure TMainForm.btnQueryOperationsClick(Sender: TObject);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  lCount: Int64;
begin
  Log('');
  Log('** Query Operations with Repository');

  lRepo := TCustomerRepository.Create;

  // Get All
  lCustomers := lRepo.GetAll;
  try
    Log(Format('GetAll returned %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  // Get Where
  lCustomers := lRepo.GetWhere('city = ?', ['Rome']);
  try
    Log(Format('GetWhere(city=Rome) returned %d customers', [lCustomers.Count]));
    for lCustomer in lCustomers do
      Log('  ' + lCustomer.ToString);
  finally
    lCustomers.Free;
  end;

  // Get One By Where
  lCustomer := lRepo.GetOneByWhere('city = ? AND rating = ?', ['Rome', 5], False);
  if Assigned(lCustomer) then
  try
    Log('GetOneByWhere found: ' + lCustomer.ToString);
  finally
    lCustomer.Free;
  end
  else
    Log('GetOneByWhere: No customer found with city=Rome and rating=5');

  // Count
  lCount := lRepo.Count;
  Log(Format('Total customers count: %d', [lCount]));
end;

procedure TMainForm.btnRQLOperationsClick(Sender: TObject);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  lCount: Int64;
begin
  Log('');
  Log('** RQL Operations with Repository');

  lRepo := TCustomerRepository.Create;

  // SelectRQL with filter
  lCustomers := lRepo.SelectRQL('eq(city,"Rome")', 100);
  try
    Log(Format('SelectRQL returned %d customers from Rome', [lCustomers.Count]));
    for lCustomer in lCustomers do
      Log('  ' + lCustomer.ToString);
  finally
    lCustomers.Free;
  end;

  // SelectRQL with sort
  lCustomers := lRepo.SelectRQL('sort(+code)', 100);
  try
    Log(Format('SelectRQL with sort returned %d customers (sorted by code)', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  // CountRQL
  lCount := lRepo.CountRQL('ge(rating,4)');
  Log(Format('CountRQL (rating >= 4): %d customers', [lCount]));

  // GetOneByRQL
  lCustomer := lRepo.GetOneByRQL('eq(rating,5)', False);
  if Assigned(lCustomer) then
  try
    Log('GetOneByRQL found: ' + lCustomer.ToString);
  finally
    lCustomer.Free;
  end
  else
    Log('GetOneByRQL: No customer found with rating=5');
end;

procedure TMainForm.btnNamedQueriesClick(Sender: TObject);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  lCount: Int64;
begin
  Log('');
  Log('** Named Queries with Repository');

  lRepo := TCustomerRepository.Create;

  // Named SQL Query
  lCustomers := lRepo.SelectByNamedQuery('BestCustomers', [], []);
  try
    Log(Format('Named Query "BestCustomers" returned %d customers', [lCustomers.Count]));
    for lCustomer in lCustomers do
      Log('  ' + lCustomer.ToString);
  finally
    lCustomers.Free;
  end;

  // Named SQL Query with parameters
  lCustomers := lRepo.SelectByNamedQuery('WithRatingGtOrEqTo', [4], [ftInteger]);
  try
    Log(Format('Named Query "WithRatingGtOrEqTo(4)" returned %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  // Named RQL Query
  lCustomers := lRepo.SelectRQLByNamedQuery('RatingEqualsToPar', [5], 100);
  try
    Log(Format('Named RQL Query "RatingEqualsToPar(5)" returned %d customers', [lCustomers.Count]));
    for lCustomer in lCustomers do
      Log('  ' + lCustomer.ToString);
  finally
    lCustomers.Free;
  end;

  // Count via Named RQL Query
  lCount := lRepo.CountRQLByNamedQuery('RatingEqualsToPar', [4]);
  Log(Format('CountRQLByNamedQuery "RatingEqualsToPar(4)": %d customers', [lCount]));
end;

procedure TMainForm.btnCustomMethodsClick(Sender: TObject);
var
  lRepo: TCustomerRepository;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('** Custom Repository Methods');

  lRepo := TCustomerRepository.Create;
  try
    // Custom method: GetCustomersByCity
    lCustomers := lRepo.GetCustomersByCity('London');
    try
      Log(Format('GetCustomersByCity("London") returned %d customers', [lCustomers.Count]));
      for lCustomer in lCustomers do
        Log('  ' + lCustomer.ToString);
    finally
      lCustomers.Free;
    end;

    // Custom method: GetTopRatedCustomers
    lCustomers := lRepo.GetTopRatedCustomers;
    try
      Log(Format('GetTopRatedCustomers returned %d customers', [lCustomers.Count]));
      for lCustomer in lCustomers do
        Log('  ' + lCustomer.ToString);
    finally
      lCustomers.Free;
    end;
  finally
    lRepo.Free;
  end;
end;

procedure TMainForm.btnAdvancedRepositoryClick(Sender: TObject);
var
  lConnection: TFDConnection;
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('** Advanced Repository with Custom Connection');

  // Create a custom connection
  lConnection := TFDConnection.Create(nil);
  try
    lConnection.ConnectionDefName := FDConnectionConfigU.CON_DEF_NAME;
    lConnection.Connected := True;

    // Create repository with custom connection
    lRepo := TMVCRepositoryWithConnection<TCustomer>.Create(lConnection, False);

    lCustomers := lRepo.GetWhere('rating >= ?', [4]);
    try
      Log(Format('Repository with custom connection returned %d customers', [lCustomers.Count]));
      for lCustomer in lCustomers do
        Log('  ' + lCustomer.ToString);
    finally
      lCustomers.Free;
    end;
  finally
    lConnection.Free;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  lEngine: TRDBMSEngine;
begin
  if not TEngineChoiceForm.Execute(lEngine) then
  begin
    Close;
    Exit;
  end;

  case lEngine of
    TRDBMSEngine.PostgreSQL:
      FDConnectionConfigU.CreatePostgresqlPrivateConnDef(True);
    TRDBMSEngine.Firebird:
      FDConnectionConfigU.CreateFirebirdPrivateConnDef(True);
    TRDBMSEngine.Interbase:
      FDConnectionConfigU.CreateInterbasePrivateConnDef(True);
    TRDBMSEngine.MSSQLServer:
      FDConnectionConfigU.CreateMSSQLServerPrivateConnDef(True);
    TRDBMSEngine.MySQL, TRDBMSEngine.MariaDB:
      FDConnectionConfigU.CreateMySQLPrivateConnDef(True);
    TRDBMSEngine.SQLite:
      FDConnectionConfigU.CreateSqlitePrivateConnDef(True);
  else
    raise Exception.Create('Unknown RDBMS');
  end;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(FDConnectionConfigU.CON_DEF_NAME);
  Caption := Caption + ' (Curr Backend: ' + ActiveRecordConnectionsRegistry.GetCurrentBackend + ')';
  LoadSampleData;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
end;

procedure TMainForm.btnClearLogClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TMainForm.btnRunAllShowcasesClick(Sender: TObject);
var
  lLog: TProc<string>;
begin
  Memo1.Clear;

  // Create anonymous method wrapper
  lLog := procedure(s: string) begin Log(s); end;

  lLog('========================================');
  lLog('RUNNING ALL REPOSITORY SHOWCASES');
  lLog('========================================');
  lLog('');

  // Basic CRUD Operations
  TAllShowcases.ShowBasicCRUD(lLog);
  TAllShowcases.ShowStore(lLog);
  TAllShowcases.ShowRefresh(lLog);

  // Query Operations
  TAllShowcases.ShowGetAll(lLog);
  TAllShowcases.ShowGetWhere(lLog);
  TAllShowcases.ShowGetOneByWhere(lLog);
  TAllShowcases.ShowGetFirstByWhere(lLog);

  // RQL Operations
  TAllShowcases.ShowSelectRQL(lLog);
  TAllShowcases.ShowCountRQL(lLog);
  TAllShowcases.ShowDeleteRQL(lLog);
  TAllShowcases.ShowGetOneByRQL(lLog);

  // SQL Operations
  TAllShowcases.ShowSelectSQL(lLog);
  TAllShowcases.ShowSelectOne(lLog);

  // Named Queries
  TAllShowcases.ShowNamedSQLQuery(lLog);
  TAllShowcases.ShowNamedRQLQuery(lLog);

  // Utility Operations
  TAllShowcases.ShowCount(lLog);
  TAllShowcases.ShowDeleteAll(lLog);
  TAllShowcases.ShowExists(lLog);

  // Custom Repository Methods
  TAllShowcases.ShowCustomMethods(lLog);

  // Advanced Features
  TAllShowcases.ShowCustomConnection(lLog);
  TAllShowcases.ShowMultipleRepositories(lLog);
  TAllShowcases.ShowRepositoryComposition(lLog);

  // Error Handling & Border Cases
  TAllShowcases.ShowNilEntityProtection(lLog);
  TAllShowcases.ShowNotFoundHandling(lLog);
  TAllShowcases.ShowEmptyResultSets(lLog);
  TAllShowcases.ShowNullableFields(lLog);
  TAllShowcases.ShowValidationErrors(lLog);

  // Performance Tests
  TAllShowcases.ShowBulkInsert(lLog);
  TAllShowcases.ShowBulkUpdate(lLog);
  TAllShowcases.ShowBulkDelete(lLog);

  // Special Cases
  TAllShowcases.ShowTransactionSupport(lLog);
  TAllShowcases.ShowReadOnlyRepository(lLog);
  TAllShowcases.ShowVersionedEntities(lLog);

  lLog('');
  lLog('========================================');
  lLog('ALL SHOWCASES COMPLETED!');
  lLog('========================================');
end;

end.
