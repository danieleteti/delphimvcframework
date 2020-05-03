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
  Data.DB, FireDAC.Comp.Client, MVCFramework.Nullables;

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
    btnNullTest: TButton;
    btnCRUDNoAutoInc: TButton;
    btnCRUDWithStringPKs: TButton;
    btnWithSpaces: TButton;
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
    procedure btnNullablesClick(Sender: TObject);
    procedure btnNullTestClick(Sender: TObject);
    procedure btnCRUDNoAutoIncClick(Sender: TObject);
    procedure btnCRUDWithStringPKsClick(Sender: TObject);
    procedure btnWithSpacesClick(Sender: TObject);
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
  Log('** Simple CRUD test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' + TCustomer.ClassName);
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Note := 'Hello there!';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Log('Just inserted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert(not lCustomer.Code.HasValue);
    lCustomer.Code.Value := '5678';
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678';
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '9012';
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

procedure TMainForm.btnCRUDNoAutoIncClick(Sender: TObject);
var
  lCustomer: TCustomerPlain;
  lID: Integer;
  I: Integer;
begin
  Log('** Simple CRUD (no autoinc) test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomerPlain>().ToString + ' row/s for entity ' +
    TCustomerPlain.ClassName);
  TMVCActiveRecord.DeleteAll(TCustomerPlain);
  Log('Deleting all entities ' + TCustomerPlain.ClassName);
  for I := 1 to 100 do
  begin
    lCustomer := TCustomerPlain.Create;
    try
      lCustomer.ID := I;
      // just for test!!
      case I mod 3 of
        0:
          lCustomer.CompanyName := 'Google Inc.';
        1:
          lCustomer.CompanyName := 'bit Time Professionals';
        2:
          lCustomer.CompanyName := 'Walt Disney Corp.';
      end;
      lCustomer.City := 'Montain View, CA';
      lCustomer.Note := 'Hello there!';
      lCustomer.Insert;
      lID := lCustomer.ID;
      Log('Just inserted Customer ' + lID.ToString);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerPlain>().ToString + ' row/s for entity ' +
    TCustomerPlain.ClassName);
  TMVCActiveRecord.DeleteRQL(TCustomerPlain, 'lt(id,90)');

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerPlain>(lID);
  try
    Assert(not lCustomer.Code.HasValue);
    lCustomer.Code.Value := '5678';
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678';
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomerPlain.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '9012';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerPlain>(lID);
  try
    lCustomer.Delete;
    Log('Just deleted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnCRUDWithStringPKsClick(Sender: TObject);
var
  lCustomer: TCustomerWithCode;
  lCode: string;
  I: Integer;
begin
  Log('** Simple CRUD (with string pks) test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomerWithCode>().ToString + ' row/s for entity ' +
    TCustomerWithCode.ClassName);
  TMVCActiveRecord.DeleteAll(TCustomerWithCode);
  Log('Deleting all entities ' + TCustomerWithCode.ClassName);
  for I := 1 to 100 do
  begin
    lCustomer := TCustomerWithCode.Create;
    try
      lCustomer.Code := I.ToString.PadLeft(4, '0');
      // just for test!!
      case I mod 3 of
        0:
          lCustomer.CompanyName := 'Google Inc.';
        1:
          lCustomer.CompanyName := 'bit Time Professionals';
        2:
          lCustomer.CompanyName := 'Walt Disney Corp.';
      end;
      lCustomer.City := 'Montain View, CA';
      lCustomer.Note := 'Hello there!';
      lCustomer.Insert;
      lCode := lCustomer.Code.Value;
      Log('Just inserted Customer ' + lCode);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerWithCode>().ToString + ' row/s for entity ' +
    TCustomerPlain.ClassName);
  TMVCActiveRecord.DeleteRQL(TCustomerWithCode, 'lt(code,"0090")');

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>(lCode);
  try
    Assert(lCustomer.Code.HasValue);
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Note changed!';
    lCustomer.Update;
    Log('Just updated Customer ' + lCode);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomerWithCode.Create;
  try
    lCustomer.LoadByPK(lCode);
    lCustomer.CompanyName := 'My New Company!';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>(lCode);
  try
    lCustomer.Delete;
    Log('Just deleted Customer ' + lCode);
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnInheritanceClick(Sender: TObject);
var
  lCustomerEx: TCustomerEx;
begin
  Log('** Inheritace test');
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
  Log('** Multithreading test');
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
            lCustomer.CompanyName := Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
              CompanySuffix[Random(high(CompanySuffix) + 1)]]);
            lCustomer.Note := lCustomer.CompanyName + ' is from ' + lCustomer.City;
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

procedure TMainForm.btnNullablesClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Log('** Nullables Test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' + TCustomer.ClassName);
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Note := 'Hello there!';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Assert(not lCustomer.Code.HasValue);
    Log('Just inserted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert(not lCustomer.Code.HasValue);
    Assert(not lCustomer.Rating.HasValue);
    Assert(lCustomer.Rating.ValueOrDefault = 0);
    lCustomer.Code.Value := '5678';
    lCustomer.Rating.Value := 3;
    Assert(lCustomer.Code.HasValue);
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678';
    lCustomer.Update;
    Assert(lCustomer.Code.HasValue);
    Assert(lCustomer.Rating.HasValue);
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert(lCustomer.Code.HasValue);
    Assert(lCustomer.Rating.HasValue);
    Assert(lCustomer.Code.ValueOrDefault = '5678');
    Assert(lCustomer.Rating.ValueOrDefault = 3);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '9012';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnNullTestClick(Sender: TObject);
var
  lTest: TNullablesTest;
  lCustomer: TCustomer;
  lID: Integer;
begin
  Log('** Nullables Test');
  TMVCActiveRecord.DeleteAll(TNullablesTest);

  lTest := TNullablesTest.Create();
  try
    lTest.f_int2 := 2;
    lTest.f_int4 := 4;
    lTest.f_int8 := 8;
    lTest.f_blob := TStringStream.Create('Hello World');
    lTest.Insert;
    Log('Inserting nulls');
  finally
    lTest.Free;
  end;

  Log('Loading records with nulls');
  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [2]);
  try
    Assert(lTest.f_int2.HasValue);
    Assert(lTest.f_int4.HasValue);
    Assert(lTest.f_int8.HasValue);
    Assert(not lTest.f_string.HasValue);
    Assert(not lTest.f_bool.HasValue);
    Assert(not lTest.f_date.HasValue);
    Assert(not lTest.f_time.HasValue);
    Assert(not lTest.f_datetime.HasValue);
    Assert(not lTest.f_float4.HasValue);
    Assert(not lTest.f_float8.HasValue);
    Assert(not lTest.f_bool.HasValue);
    Assert(Assigned(lTest));
    lTest.f_int2 := lTest.f_int2.Value + 2;
    lTest.f_int4 := lTest.f_int4.Value + 4;
    lTest.f_int8 := lTest.f_int8.Value + 8;
    lTest.f_blob.Free;
    lTest.f_blob := nil;
    lTest.Update;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [4]);
  try
    Assert(lTest.f_int2.ValueOrDefault = 4);
    Assert(lTest.f_int4.ValueOrDefault = 8);
    Assert(lTest.f_int8.ValueOrDefault = 16);
    Assert(not lTest.f_string.HasValue);
    Assert(not lTest.f_bool.HasValue);
    Assert(not lTest.f_date.HasValue);
    Assert(not lTest.f_time.HasValue);
    Assert(not lTest.f_datetime.HasValue);
    Assert(not lTest.f_float4.HasValue);
    Assert(not lTest.f_float8.HasValue);
    Assert(not lTest.f_bool.HasValue);
    Assert(not Assigned(lTest.f_blob), 'Blob contains a value when should not');
    TMVCActiveRecord.DeleteRQL(TNullablesTest, 'eq(f_int2,4)');
  finally
    lTest.Free;
  end;

  Assert(TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = 4', [], False) = nil);

  lTest := TNullablesTest.Create;
  try
    lTest.f_int2 := 2;
    lTest.f_int4 := 4;
    lTest.f_int8 := 8;
    lTest.f_string := 'Hello World';
    lTest.f_bool := True;
    lTest.f_date := EncodeDate(2020, 02, 01);
    lTest.f_time := EncodeTime(12, 24, 36, 0);
    lTest.f_datetime := Now;
    lTest.f_float4 := 1234.5678;
    lTest.f_float8 := 12345678901234567890.0123456789;
    lTest.f_currency := 1234567890.1234;
    lTest.Insert;
  finally
    lTest.Free;
  end;

  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' + TCustomer.ClassName);
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Note := 'Hello there!';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Assert(not lCustomer.Code.HasValue);
    Log('Just inserted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert(not lCustomer.Code.HasValue);
    Assert(not lCustomer.Rating.HasValue);
    Assert(lCustomer.Rating.ValueOrDefault = 0);
    lCustomer.Code.Value := '5678';
    lCustomer.Rating.Value := 3;
    Assert(lCustomer.Code.HasValue);
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678';
    lCustomer.Update;
    Assert(lCustomer.Code.HasValue);
    Assert(lCustomer.Rating.HasValue);
    Log('Just updated Customer ' + lID.ToString + ' with nulls');
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert(lCustomer.Code.HasValue);
    Assert(lCustomer.Rating.HasValue);
    Assert(lCustomer.Code.ValueOrDefault = '5678');
    Assert(lCustomer.Rating.ValueOrDefault = 3);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '9012';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

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
  Log('** Relations test');
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
  Log('** RQL Queries Test');
  Log('>> RQL Query (1) - ' + cRQL1);
  lList := TMVCActiveRecord.SelectRQL(TCustomer, cRQL1, 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault,
        lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

  Log('>> RQL Query (2) - ' + cRQL2);
  lCustList := TMVCActiveRecord.SelectRQL<TCustomer>(cRQL2, 20);
  try
    Log(lCustList.Count.ToString + ' record/s found');
    for lCustomer in lCustList do
    begin
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault,
        lCustomer.City]));
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
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault,
        lCustomer.City]));
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
  lID: NullableInt64;
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
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'interbase' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'mssql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else
    raise Exception.Create('Unsupported backend: ' + ActiveRecordConnectionsRegistry.GetCurrentBackend);

  try
    for lCustomer in lCustomers do
    begin
      Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault]));
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

  lDS := TMVCActiveRecord.SelectDataSet
    ('SELECT * FROM orders o join customers c on c.id = o.id_customer where o.order_date >= ?', [Date - 5000],
    [ftDate]);
  try
    while not lDS.Eof do
    begin
      Log(Format('OrderDate: %12s - Customer: %s', [datetostr(lDS.FieldByName('order_date').AsDateTime),
        lDS.FieldByName('description').AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

  lDS := TMVCActiveRecord.SelectDataSet
    ('SELECT * FROM orders o left join customers c on c.id = o.id_customer where o.order_date >= ? and c.id > ?',
    [Date - 5000, 1],
    [ftDate]);
  try
    while not lDS.Eof do
    begin
      Log(Format('OrderDate: %12s - Customer: %s', [datetostr(lDS.FieldByName('order_date').AsDateTime),
        lDS.FieldByName('description').AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

  Log('** GetFirstByWhere');
  lCustomer := TMVCActiveRecord.GetFirstByWhere<TCustomer>('id > ?', [1]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault]));
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetFirstByWhere<TCustomer>('id > ?', [1], [ftInteger]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault]));
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  Log('** GetOneByWhere');
  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID.Value]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault]));
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID.Value], [ftInteger]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault, lCustomer.CompanyName.ValueOrDefault]));
  finally
    lCustomer.Free;
  end;

end;

procedure TMainForm.btnTransientFieldsClick(Sender: TObject);
var
  lCustomer: TCustomerWithTransient;
  lID: Integer;
begin
  Log('** CRUD test with transient fields');
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
  Log('** Validation test (some exceptions will be raised)');
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

procedure TMainForm.btnWithSpacesClick(Sender: TObject);
var
  lCustomer: TCustomerWithSpaces;
  lID: Integer;
  I: Integer;
begin
  Log('** Simple CRUD (table and fields with spaces) test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomerWithSpaces>().ToString + ' row/s for entity ' +
    TCustomerWithSpaces.ClassName);
  TMVCActiveRecord.DeleteAll(TCustomerWithSpaces);
  Log('Deleting all entities ' + TCustomerWithSpaces.ClassName);
  for I := 1 to 100 do
  begin
    lCustomer := TCustomerWithSpaces.Create;
    try
      lCustomer.ID := I;
      // just for test!!
      case I mod 3 of
        0:
          lCustomer.CompanyName := 'Google Inc.';
        1:
          lCustomer.CompanyName := 'bit Time Professionals';
        2:
          lCustomer.CompanyName := 'Walt Disney Corp.';
      end;
      lCustomer.City := 'Montain View, CA';
      lCustomer.Note := 'Hello there!';
      lCustomer.Insert;
      lID := lCustomer.ID;
      Log('Just inserted Customer ' + lID.ToString);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerWithSpaces>().ToString + ' row/s for entity ' +
    TCustomerWithSpaces.ClassName);
  TMVCActiveRecord.DeleteRQL(TCustomerWithSpaces, 'lt(id,90)');

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID);
  try
    Assert(not lCustomer.Code.HasValue);
    lCustomer.Code.Value := '5678';
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678';
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomerWithSpaces.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '9012';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID);
  try
    lCustomer.Delete;
    Log('Just deleted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
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
        FDConnectionConfigU.CreateInterbasePrivateConnDef(True);
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
        FDConnectionConfigU.CreateMSSQLServerPrivateConnDef(True);
      end;
  else
    raise Exception.Create('Unknown RDBMS');
  end;

  FDConnection1.Params.Clear;
  FDConnection1.ConnectionDefName := FDConnectionConfigU.CON_DEF_NAME;
  FDConnection1.Connected := True;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(FDConnection1);
  Caption := Caption + ' (Curr Backend: ' + ActiveRecordConnectionsRegistry.GetCurrentBackend + ')';
{$IFDEF USE_SEQUENCES}
  Caption := Caption + ' USE_SEQUENCES';
{$ELSE}
  Caption := Caption + ' WITHOUT SEQUENCES';
{$ENDIF}
  btnWithSpaces.Enabled := ActiveRecordConnectionsRegistry.GetCurrentBackend = 'postgresql';
end;

procedure TMainForm.Log(const Value: string);
begin
  Memo1.Lines.Add(Value);
  Memo1.Update;
end;

end.
