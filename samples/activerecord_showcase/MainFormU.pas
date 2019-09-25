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
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client;

type
  TMainForm = class(TForm)
    btnCRUD: TButton;
    btnSelect: TButton;
    Memo1: TMemo;
    btnRelations: TButton;
    btnInheritance: TButton;
    btnValidation: TButton;
    btnMultiThreading: TButton;
    btnRQL: TButton;
    btnTransientFields: TButton;
    FDConnection1: TFDConnection;
    procedure btnCRUDClick(Sender: TObject);
    procedure btnInheritanceClick(Sender: TObject);
    procedure btnMultiThreadingClick(Sender: TObject);
    procedure btnRelationsClick(Sender: TObject);
    procedure btnRQLClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnValidationClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTransientFieldsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Log(const Value: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MVCFramework.ActiveRecord,
  EntitiesU,
  System.Threading,
  System.Generics.Collections,
  MVCFramework.DataSet.Utils,
  MVCFramework.RQL.Parser,
  System.Math,
  FDConnectionConfigU, EngineChoiceFormU;

procedure TMainForm.btnCRUDClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  ShowMessage('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' + TCustomer.ClassName);

  Log('Simple CRUD test');
  lCustomer := TCustomer.Create;
  try
    lCustomer.Code := '1234';
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Log('Just inserted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    lCustomer.Code := '5678';
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code := '9012';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    lCustomer.Delete;
    Log('Just deleted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnInheritanceClick(Sender: TObject);
var
  lCustomerEx: TCustomerEx;
begin
  lCustomerEx := TCustomerEx.Create;
  try
    lCustomerEx.LoadByPK(1);
  finally
    lCustomerEx.Free;
  end;
end;

procedure TMainForm.btnMultiThreadingClick(Sender: TObject);
var
  lTasks: TArray<ITask>;
  lProc: TProc;
  lConnParams: string;
const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');
begin
  TMVCActiveRecord.DeleteRQL(TCustomer, 'in(City,["Rome","New York","London","Melbourne","Berlin"])');

  lConnParams := FDConnection1.Params.Text;
  lProc := procedure
    var
      lConn: TFDConnection;
      lCustomer: TCustomer;
      I: Integer;
    begin
      lConn := TFDConnection.Create(nil);
      try
        lConn.ConnectionDefName := CON_DEF_NAME;
        ActiveRecordConnectionsRegistry.AddConnection('default', lConn, True);
        lConn.Params.Text := lConnParams;
        lConn.Open;
        for I := 1 to 30 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
            lCustomer.City := Cities[Random(high(Cities) + 1)];
            lCustomer.CompanyName := Format('%s %s %s', [lCustomer.City, Stuff[Random(High(Stuff) + 1)],
              CompanySuffix[Random(High(CompanySuffix) + 1)]]);
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveConnection('default');
      end;
    end;

  lTasks := [TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc)];
  TTask.WaitForAll(lTasks);

  ShowMessage('Just inserted ' + TMVCActiveRecord.Count(TCustomer,
    'in(City,["Rome","New York","London","Melbourne","Berlin"])').ToString + ' records');
end;

procedure TMainForm.btnRelationsClick(Sender: TObject);
var
  lCustomer: TCustomerEx;
  lOrder: TOrder;
  lOrderRows: TObjectList<TOrderDetail>;
  lOrderRow: TOrderDetail;
  lOrderDetail: TOrderDetail;
  I: Integer;
  j: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomerEx);

  lCustomer := TCustomerEx.Create;
  try
    lCustomer.Code := '001';
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.Insert;
    for I := 1 to 3 do
    begin
      lCustomer.Orders.Add(TOrder.Create);
      lCustomer.Orders.Last.CustomerID := lCustomer.ID;
      lCustomer.Orders.Last.OrderDate := EncodeDate(2018, 5 + I, 20 + I);
      lCustomer.Orders.Last.Total := I * 3;
      lCustomer.Orders.Last.Insert;

      for j := 1 to 4 do
      begin
        lOrderDetail := TOrderDetail.Create;
        try
          lOrderDetail.OrderID := lCustomer.Orders.Last.ID;
          lOrderDetail.ArticleID := j;
          lOrderDetail.Price := Random(j * 10);
          lOrderDetail.Discount := j;
          lOrderDetail.Quantity := j * 2;
          lOrderDetail.Description := 'MY PRODUCT ' + I.ToString + '/' + j.ToString;
          lOrderDetail.Total := j * j * j;
          lOrderDetail.Insert;
        finally
          lOrderDetail.Free;
        end;
      end;
    end;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerEx>('Code = ?', ['001']);
  try
    Log(lCustomer.CompanyName);
    for lOrder in lCustomer.Orders do
    begin
      Log(Format('  %5.5d - %s - %m', [lOrder.ID, datetostr(lOrder.OrderDate), lOrder.Total]));
      lOrderRows := TMVCActiveRecord.Where<TOrderDetail>('id_order = ?', [lOrder.ID]);
      try
        for lOrderRow in lOrderRows do
        begin
          Log(Format('         %-20s - %4d - %m', [lOrderRow.Description, lOrderRow.Quantity, lOrder.Total]));
        end;
        Log('');
      finally
        lOrderRows.Free;
      end;
    end;
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnRQLClick(Sender: TObject);
var
  lList: TMVCActiveRecordList;
  lItem: TMVCActiveRecord;
  lCustomer: TCustomer;
  lCustList: TObjectList<TCustomer>;
const
  cRQL1 = 'in(City,["Rome","London"]);sort(+code);limit(0,50)';
  cRQL2 = 'and(eq(City,"Rome"),or(contains(CompanyName,"GAS"),contains(CompanyName,"Motors")))';
begin
  Log('**RQL Query (1) - ' + cRQL1);
  lList := TMVCActiveRecord.SelectRQL(TCustomer, cRQL1, 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code, lCustomer.CompanyName, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

  Log('**RQL Query (2) - ' + cRQL2);
  lCustList := TMVCActiveRecord.SelectRQL<TCustomer>(cRQL2, 20);
  try
    Log(lCustList.Count.ToString + ' record/s found');
    for lCustomer in lCustList do
    begin
      Log(Format('%5s - %s (%s)', [lCustomer.Code, lCustomer.CompanyName, lCustomer.City]));
    end;
  finally
    lCustList.Free;
  end;

  Log('**RQL Query (3) - ' + cRQL2);
  lList := TMVCActiveRecord.SelectRQL(TCustomer, cRQL2, 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code, lCustomer.CompanyName, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

end;

procedure TMainForm.btnSelectClick(Sender: TObject);
var
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  lDS: TDataSet;
begin
  Log('** Query SQL');
  // Bypassing the RQL parser you can use DBMS-specific features or just joining your tables.
  // This is just a sample, you can do the "select" also using the RQL engine
  if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'firebird' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description CONTAINING ?',
      ['google'])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'mysql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'postgresql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description ILIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'sqlite' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else
    raise Exception.Create('Unsupported backend: ' + ActiveRecordConnectionsRegistry.GetCurrentBackend);

  try
    for lCustomer in lCustomers do
    begin
      Log(Format('%8.5s - %s', [lCustomer.Code, lCustomer.CompanyName]));
    end;
  finally
    lCustomers.Free;
  end;

  Log('** Query SQL returning DataSet');
  lDS := TMVCActiveRecord.SelectDataSet('SELECT * FROM customers', []);
  try
    while not lDS.Eof do
    begin
      Log(Format('%8.5s - %s', [lDS.FieldByName('code').AsString, lDS.FieldByName('description').AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

end;

procedure TMainForm.btnTransientFieldsClick(Sender: TObject);
var
  lCustomer: TCustomerWithTransient;
  lID: Integer;
begin
  Log('Transient CRUD test');
  lCustomer := TCustomerWithTransient.Create;
  try
    {
      'Code' and City will not be persisted because defined as 'transient'
    }
    lCustomer.Code := '1234';
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Log('Just inserted "transient" Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithTransient>(lID);
  try
    lCustomer.CompanyName := lCustomer.CompanyName + ' changed!';
    lCustomer.Code := 'this code will not be saved';
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithTransient>(lID);
  try
    lCustomer.Delete;
    Log('Just deleted "transient" Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnValidationClick(Sender: TObject);
var
  lCustomer: TCustomerWithLogic;
  lID: Integer;
begin
  lCustomer := TCustomerWithLogic.Create;
  try
    lCustomer.Code := '1234';
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome';
    lCustomer.Insert;
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithLogic>(lID);
  try
    Log(lCustomer.CompanyName + ' => IsLocatedInRome: ' + BoolToStr(lCustomer.IsLocatedInRome, True));
    lCustomer.Code := '';
    lCustomer.Update; // raise exception
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ActiveRecordConnectionsRegistry.RemoveConnection('default');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  case TEngineChoiceForm.Execute of
    TRDBMSEngine.PostgreSQL:
      begin
        FDConnectionConfigU.CreatePostgresqlPrivateConnDef(True);
      end;
    TRDBMSEngine.Firebird:
      begin
        FDConnectionConfigU.CreateFirebirdPrivateConnDef(True);
      end;
    TRDBMSEngine.Interbase:
      begin
        raise Exception.Create('This DEMO doesn''t support Interbase (while the framework does)');
      end;
    TRDBMSEngine.MySQL:
      begin
        FDConnectionConfigU.CreateMySQLPrivateConnDef(True);
      end;
    TRDBMSEngine.MariaDB:
      begin
        FDConnectionConfigU.CreateMySQLPrivateConnDef(True);
      end;
    TRDBMSEngine.SQLite:
      begin
        FDConnectionConfigU.CreateSqlitePrivateConnDef(True);
      end;
    TRDBMSEngine.MSSQLServer:
      begin
        // FDConnectionConfigU.CreatePostgresqlPrivateConnDef(True);
        raise Exception.Create('This DEMO doesn''t support MSSQLServer (while the framework does)');
      end;
  else
    raise Exception.Create('Unknown RDBMS');
  end;

  FDConnection1.Params.Clear;
  FDConnection1.ConnectionDefName := FDConnectionConfigU.CON_DEF_NAME;
  FDConnection1.Connected := True;

  ActiveRecordConnectionsRegistry.AddConnection('default', FDConnection1);
  Caption := Caption + ' (Curr Backend: ' + ActiveRecordConnectionsRegistry.GetCurrentBackend + ')';
end;

procedure TMainForm.Log(const Value: string);
begin
  Memo1.Lines.Add(Value);
  // Memo1.Update;
end;

end.
