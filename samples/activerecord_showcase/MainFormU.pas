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
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef;

type
  TMainForm = class(TForm)
    btnCRUD: TButton;
    FDConnection1: TFDConnection;
    btnSelect: TButton;
    Memo1: TMemo;
    btnRelations: TButton;
    btnInheritance: TButton;
    btnValidation: TButton;
    btnMultiThreading: TButton;
    btnRQL: TButton;
    procedure btnCRUDClick(Sender: TObject);
    procedure btnInheritanceClick(Sender: TObject);
    procedure btnMultiThreadingClick(Sender: TObject);
    procedure btnRelationsClick(Sender: TObject);
    procedure btnRQLClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnValidationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  FDConnectionConfigU;

procedure TMainForm.btnCRUDClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
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

  lCustomer := TMVCActiveRecord.GetByPrimaryKey<TCustomer>(lID);
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

  lCustomer := TMVCActiveRecord.GetByPrimaryKey<TCustomer>(lID);
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
const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
begin
  if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'firebird' then
    TMVCActiveRecord.CurrentConnection.ExecSQL('DELETE FROM CLIENTI WHERE RAG_SOC STARTING ''Company ''')
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'mysql' then
    TMVCActiveRecord.CurrentConnection.ExecSQL('DELETE FROM CLIENTI WHERE RAG_SOC LIKE ''Company %''')
  else
    raise Exception.Create('Unknown backend for direct SQL execution');

  lProc :=
      procedure
    var
      lConn: TFDConnection;
      lCustomer:
        TCustomer;
      I:
        Integer;
    begin
      lConn := TFDConnection.Create(nil);
      try
        lConn.ConnectionDefName := CON_DEF_NAME;
        ActiveRecordConnectionsRegistry.AddConnection('default', lConn);
        lConn.Params.Assign(FDConnection1.Params);
        lConn.Open;
        for I := 1 to 10 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.Current.ThreadID, I]);
            lCustomer.CompanyName := Format('Company %5.5d', [Random(99999)]);
            lCustomer.City := Cities[Random(high(Cities) + 1)];
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveConnection('default');
      end;
    end;

  lTasks := [
    TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    , TTask.Run(lProc)
    ];
  TTask.WaitForAll(lTasks);
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
  TCustomerEx.CurrentConnection.ExecSQL('DELETE FROM CLIENTI');

  lOrder := TOrder.Create;
  try
    lOrder.CustomerID := 123;
    lOrder.OrderDate := EncodeDate(2018, 10, 20);
    lOrder.Total := 1234;
    lOrder.Insert;
  finally
    lOrder.Free;
  end;

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

  lCustomer := TCustomerEx.GetOneByWhere<TCustomerEx>('Codice = ?', ['001']);
  try
    Log(lCustomer.CompanyName);
    for lOrder in lCustomer.Orders do
    begin
      Log(Format('  %5.5d - %s - %m', [lOrder.ID, datetostr(lOrder.OrderDate), lOrder.Total]));
      lOrderRows := TOrderDetail.Where<TOrderDetail>('ID_ORDINE = ?', [lOrder.ID]);
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
begin
  lList := TMVCActiveRecord.SelectRQL(TCustomer, 'eq(City,"Rome")', 20);
  try
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
  if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'firebird' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM CLIENTI WHERE RAG_SOC CONTAINING ?', ['google'])
  else
    lCustomers := TMVCActiveRecord.Select<TCustomer>('SELECT * FROM CLIENTI WHERE RAG_SOC LIKE ''%google%''', []);
  try
    for lCustomer in lCustomers do
    begin
      Log(Format('%8.5s - %s', [lCustomer.Code, lCustomer.CompanyName]));
    end;
  finally
    lCustomers.Free;
  end;

  Log('** Query SQL returning DataSet');
  if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'firebird' then
    lDS := TMVCActiveRecord.SelectDataSet('SELECT * FROM CLIENTI WHERE RAG_SOC CONTAINING ?', ['google'])
  else
    lDS := TMVCActiveRecord.SelectDataSet('SELECT * FROM CLIENTI WHERE RAG_SOC LIKE ''%google%''', []);
  try
    while not lDS.Eof do
    begin
      Log(Format('%8.5s - %s', [lDS.FieldByName('CODICE').AsString, lDS.FieldByName('RAG_SOC').AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
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

  lCustomer := TCustomer.GetByPrimaryKey<TCustomerWithLogic>(lID);
  try
    Log(lCustomer.CompanyName + ' => IsLocatedInRome: ' + BoolToStr(lCustomer.IsLocatedInRome, True));
    lCustomer.Code := '';
    lCustomer.Update; // raise exception
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // To use Firebird uncomment the following line (and comment the next one)
  FDConnectionConfigU.CreateFirebirdPrivateConnDef(True);

  // To use MySQL uncomment the following line  (and comment the previous one)
  // FDConnectionConfigU.CreateMySQLPrivateConnDef(True);

  FDConnection1.Params.Clear;
  FDConnection1.ConnectionDefName := FDConnectionConfigU.CON_DEF_NAME;
  ActiveRecordConnectionsRegistry.AddConnection('default', FDConnection1);
end;

procedure TMainForm.Log(
  const
  Value:
  string);
begin
  Memo1.Lines.Add(Value);
  Memo1.Update;
end;

end.
