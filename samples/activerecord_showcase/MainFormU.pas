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
  MVCFramework.Nullables,
  MVCFramework.ActiveRecord,
  System.Generics.Collections, System.Diagnostics;

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
    btnReadOnlyFields: TButton;
    FDConnection1: TFDConnection;
    btnNullTest: TButton;
    btnCRUDNoAutoInc: TButton;
    btnCRUDWithStringPKs: TButton;
    btnWithSpaces: TButton;
    btnCountWithRQL: TButton;
    btnReadAndWriteOnly: TButton;
    btnClientGeneratedPK: TButton;
    btnAttributes: TButton;
    btnJSON_XML_Types: TButton;
    btnMerge: TButton;
    btnTableFilter: TButton;
    btnPartitioning: TButton;
    btnCRUDWithGUID: TButton;
    btnOOP: TButton;
    btnReadOnly: TButton;
    btnSpeed: TButton;
    btnRefresh: TButton;
    btnNamedQuery: TButton;
    btnVirtualEntities: TButton;
    procedure btnCRUDClick(Sender: TObject);
    procedure btnInheritanceClick(Sender: TObject);
    procedure btnMultiThreadingClick(Sender: TObject);
    procedure btnRelationsClick(Sender: TObject);
    procedure btnRQLClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnValidationClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReadOnlyFieldsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNullablesClick(Sender: TObject);
    procedure btnNullTestClick(Sender: TObject);
    procedure btnCRUDNoAutoIncClick(Sender: TObject);
    procedure btnCRUDWithStringPKsClick(Sender: TObject);
    procedure btnWithSpacesClick(Sender: TObject);
    procedure btnCountWithRQLClick(Sender: TObject);
    procedure btnReadAndWriteOnlyClick(Sender: TObject);
    procedure btnClientGeneratedPKClick(Sender: TObject);
    procedure btnAttributesClick(Sender: TObject);
    procedure btnJSON_XML_TypesClick(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
    procedure btnTableFilterClick(Sender: TObject);
    procedure btnPartitioningClick(Sender: TObject);
    procedure btnCRUDWithGUIDClick(Sender: TObject);
    procedure btnOOPClick(Sender: TObject);
    procedure btnReadOnlyClick(Sender: TObject);
    procedure btnSpeedClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnNamedQueryClick(Sender: TObject);
    procedure btnVirtualEntitiesClick(Sender: TObject);
  private
    procedure Log(const Value: string);
    procedure LoadCustomers;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


uses
  EntitiesU,
  System.Threading,
  MVCFramework.DataSet.Utils,
  MVCFramework.RQL.Parser,
  System.Math,
  FDConnectionConfigU,
  EngineChoiceFormU,
  System.Rtti;

const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');

procedure TMainForm.btnAttributesClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Log('** Dynamic Properties Access');
  lCustomer := TCustomer.Create;
  try
    lCustomer.Attributes['CompanyName'] := 'Google Inc.';
    lCustomer.Attributes['City'] := 'Montain View, CA';
    lCustomer.Attributes['Note'] := 'Hello there!';
    lCustomer.Attributes['Code'] := 'XX123';
    lCustomer.Attributes['Rating'] := 3;
    lCustomer.Insert;
    lID := lCustomer.ID;
    Log('Just inserted Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert('Google Inc.' = lCustomer.Attributes['CompanyName'].AsType<NullableString>().Value);
    Assert('Montain View, CA' = lCustomer.Attributes['City'].AsString);
    Assert('XX123' = lCustomer.Attributes['Code'].AsType<NullableString>().Value);
    Assert('Hello there!' = lCustomer.Attributes['Note'].AsString);
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
end;

procedure TMainForm.btnClientGeneratedPKClick(Sender: TObject);
var
  lCustomer: TCustomerPlainWithClientPK;
begin
  Log('** OnBeforeInsert and SetPK');
  lCustomer := TCustomerPlainWithClientPK.Create();
  try
    lCustomer.Store;
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnCountWithRQLClick(Sender: TObject);
var
  lRQL: string;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('** TMVCActiveRecord.Count<TCustomer>(RQL) [Just uses Filter]');

  TMVCActiveRecord.DeleteAll(TCustomer);
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

  lRQL := 'contains(city,"e")';
  Log(lRQL + ' => ' + TMVCActiveRecord.Count<TCustomer>(lRQL).ToString);

  lRQL := 'contains(city,"e");sort(+city)';
  Log(lRQL + ' => ' + TMVCActiveRecord.Count<TCustomer>(lRQL).ToString);

  lRQL := 'contains(city,"e");limit(1,1)';
  Log(lRQL + ' => ' + TMVCActiveRecord.Count<TCustomer>(lRQL).ToString);

  lRQL := 'contains(city,"e");sort(+city);limit(1,1)';
  Log(lRQL + ' => ' + TMVCActiveRecord.Count<TCustomer>(lRQL).ToString);

  lRQL := 'contains(city,"e");sort(+city);limit(0,5)';
  Log(lRQL + ' => ' + TMVCActiveRecord.Count<TCustomer>(lRQL).ToString);
end;

procedure TMainForm.btnCRUDClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
  lTestNote: string;
begin
  Log('** Simple CRUD test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' +
    TCustomer.ClassName);
  lCustomer := TCustomer.Create;
  try
    Log('Entity ' + TCustomer.ClassName + ' is mapped to table ' + lCustomer.TableName);
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Note := 'Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁';
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
    lCustomer.Note := lCustomer.Note + sLineBreak + 'Code changed to 5678 🙂';
    lTestNote := lCustomer.Note;
    lCustomer.Update;
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    lCustomer.Code.Value := '😉9012🙂';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.LoadByPK(lID);
    Assert(lCustomer.Code.Value = '😉9012🙂');
    Assert(lCustomer.Note = lTestNote);
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
      lCustomer.CreationTime := Time;
      lCustomer.CreationDate := Date;
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

procedure TMainForm.btnCRUDWithGUIDClick(Sender: TObject);
var
  lTestNote: string;
  lCustWithGUID: TCustomerWithGUID;
  lIDGUID: TGUID;
begin
  TMVCActiveRecord.DeleteAll(TCustomerWithGUID);

  Log('** Using GUID as primary key');

  lCustWithGUID := TCustomerWithGUID.Create;
  try
    Log('Entity ' + TCustomerWithGUID.ClassName + ' is mapped to table ' + lCustWithGUID.TableName);
    lCustWithGUID.GUID := TGUID.NewGuid;
    lCustWithGUID.CompanyName := 'Google Inc.';
    lCustWithGUID.City := 'Montain View, CA';
    lCustWithGUID.Note := 'Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁';
    lCustWithGUID.Insert;
    lIDGUID := lCustWithGUID.GUID;
    Log('Just inserted Customer With GUID ' + lIDGUID.ToString);
  finally
    lCustWithGUID.Free;
  end;

  lCustWithGUID := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lIDGUID);
  try
    Assert(not lCustWithGUID.Code.HasValue);
    lCustWithGUID.Code.Value := '5678';
    lCustWithGUID.Note := lCustWithGUID.Note + sLineBreak + 'Code changed to 5678 🙂';
    lTestNote := lCustWithGUID.Note;
    lCustWithGUID.Update;
    Log('Just updated Customer ' + lIDGUID.ToString);
  finally
    lCustWithGUID.Free;
  end;

  lCustWithGUID := TCustomerWithGUID.Create;
  try
    lCustWithGUID.LoadByPK(lIDGUID);
    lCustWithGUID.Code.Value := '😉9012🙂';
    lCustWithGUID.Update;
  finally
    lCustWithGUID.Free;
  end;

  lCustWithGUID := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lIDGUID);
  try
    lCustWithGUID.Delete;
    Log('Just deleted Customer ' + lIDGUID.ToString);
  finally
    lCustWithGUID.Free;
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

  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerWithCode>().ToString + ' row/s for entity '
    + TCustomerPlain.ClassName);
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

procedure TMainForm.btnJSON_XML_TypesClick(Sender: TObject);
var
  lCTypes: TComplexTypes;
  lID: Int64;
begin
  if GetBackEndByConnection(TMVCActiveRecord.CurrentConnection) = TMVCActiveRecordBackEnd.PostgreSQL then
  begin
    TMVCActiveRecord.DeleteAll(TComplexTypes);

    lCTypes := TComplexTypes.Create;
    try
      lCTypes.JSON := '{"field_type":"json"}';
      lCTypes.JSONB := '{"field_type":"jsonb"}';
      lCTypes.XML := '<field_type>xml</field_type>';
      lCTypes.Insert;
      lID := lCTypes.ID;
    finally
      lCTypes.Free;
    end;

    lCTypes := TMVCActiveRecord.GetByPK<TComplexTypes>(lID);
    try
      lCTypes.JSON := '{"field_type":"json", "updated": true}';
      lCTypes.JSONB := '{"field_type":"jsonb", "updated": true}';
      lCTypes.XML := '<field_type updated="true">xml</field_type>';
      lCTypes.Update;
    finally
      lCTypes.Free;
    end;

    Log('Executing ==> (jsonb_field ->> ''updated'')::bool = true');
    lCTypes := TMVCActiveRecord.GetFirstByWhere<TComplexTypes>('(jsonb_field ->> ''updated'')::bool = true', []);
    try
      Log('JSON ==> ' + lCTypes.JSON);
    finally
      lCTypes.Free;
    end;
  end;
end;

procedure TMainForm.btnMergeClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lCustomers: TObjectList<TCustomer>;
  lCustomersChanges: TObjectList<TCustomer>;
begin
  Log('** IMVCMultiExecutor demo');
  TMVCActiveRecord.DeleteAll(TCustomer);
  LoadCustomers;
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    lCustomersChanges := TObjectList<TCustomer>.Create(True);
    try
      //these 2 customers will be updated
      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.ID := lCustomers[0].ID;
      lCustomer.Code := 'C8765';
      lCustomer.CompanyName := '(changed) Company1';
      lCustomer.City := '(changed) City';
      lCustomer.Rating := 1;

      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.ID := lCustomers[1].ID;
      lCustomer.Code := lCustomers[1].Code;
      lCustomer.CompanyName := '(changed) Company2';
      lCustomer.City := '(changed) City';
      lCustomer.Rating := 1;


      //these 2 customer will be created
      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C9898';
      lCustomer.CompanyName := '(new) Company3';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 1;

      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C2343';
      lCustomer.CompanyName := '(new) Company4';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 1;

      //all the other customers will be deleted

      //calculate the unit-of-work to merge the lists
      var lUOW := TMVCActiveRecord.Merge<TCustomer>(lCustomers, lCustomersChanges);
      //apply the UnitOfWork
      lUOW.Apply(
        procedure (const Customer: TCustomer; const EntityAction: TMVCEntityAction; var Handled: Boolean)
        begin
          Handled := False; //set it to true to execute action manually
          case EntityAction of
            eaCreate: Log('Inserting Customer : ' + Customer.ToString);
            eaUpdate: Log('Updating Customer  : ' + Customer.ToString);
            eaDelete: Log('Deleting Customer  : ' + Customer.ToString);
          end;
        end);


    finally
      lCustomersChanges.Free;
    end;
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    Assert(lCustomers.Count = 4, 'Expected 4 customers, got ' + lCustomers.Count.ToString);
  finally
    lCustomers.Free;
  end;
end;

procedure TMainForm.btnMultiThreadingClick(Sender: TObject);
var
  lTasks: TArray<ITask>;
  lProc: TProc;
  lConnParams: string;
begin
  Log('** Multithreading test');
  TMVCActiveRecord.DeleteRQL(TCustomer,
    'in(City,["Rome","New York","London","Melbourne","Berlin"])');

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
        ActiveRecordConnectionsRegistry.AddDefaultConnection(lConn, True);
        lConn.Params.Text := lConnParams;
        lConn.Open;
        for I := 1 to 30 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
            lCustomer.City := Cities[Random(high(Cities) + 1)];
            lCustomer.CompanyName :=
              Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
              CompanySuffix[Random(high(CompanySuffix) + 1)]]);
            lCustomer.Note := lCustomer.CompanyName + ' is from ' + lCustomer.City;
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
      end;
    end;

  lTasks := [TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc)];
  TTask.WaitForAll(lTasks);

  ShowMessage('Just inserted ' + TMVCActiveRecord.Count(TCustomer,
    'in(City,["Rome","New York","London","Melbourne","Berlin"])').ToString + ' records');
end;

procedure TMainForm.btnNamedQueryClick(Sender: TObject);
begin
  Log('** Named SQL Query');
  Log('QuerySQL: RatingLessThanPar');
  var lCustomers := TMVCActiveRecord.SelectByNamedQuery<TCustomer>('RatingLessThanPar', [4], [ftInteger]);
  try
    for var lCustomer in lCustomers do
    begin
      Log(Format('%4d - %8.5s - %s', [lCustomer.ID.ValueOrDefault, lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault]));
    end;
  finally
    lCustomers.Free;
  end;

  Log('QuerySQL: RatingEqualsToPar');
  lCustomers := TMVCActiveRecord.SelectByNamedQuery<TCustomer>('RatingEqualsToPar', [3], [ftInteger]);
  try
    for var lCustomer in lCustomers do
    begin
      Log(Format('%4d - %8.5s - %s', [lCustomer.ID.ValueOrDefault, lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault]));
    end;
  finally
    lCustomers.Free;
  end;

  Log('** Named RQL Query');
  Log('QueryRQL: RatingLessThanPar');
  lCustomers := TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>('RatingLessThanPar', [4], 1000);
  try
    for var lCustomer in lCustomers do
    begin
      Log(Format('%4d - %8.5s - %s', [lCustomer.ID.ValueOrDefault, lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault]));
    end;
  finally
    lCustomers.Free;
  end;

  Log('QueryRQL: RatingEqualsToPar');
  lCustomers := TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>('RatingEqualsToPar', [3], 1000);
  try
    for var lCustomer in lCustomers do
    begin
      Log(Format('%4d - %8.5s - %s', [lCustomer.ID.ValueOrDefault, lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault]));
    end;
  finally
    lCustomers.Free;
  end;


end;

procedure TMainForm.btnNullablesClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Log('** Nullables Test');
  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' +
    TCustomer.ClassName);
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
    with TStreamWriter.Create(lTest.f_blob) do
      try
        write('Hello World');
      finally
        Free;
      end;
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
    lTest.f_int4 := lTest.f_int4.Value + 4;
    lTest.f_int8 := lTest.f_int8.Value + 8;
    lTest.f_blob.Size := 0;
    lTest.Update;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [2]);
  try
    Assert(lTest.f_int2.ValueOrDefault = 2);
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
    Assert(lTest.f_blob.Size = 0, 'Blob contains a value when should not');
    TMVCActiveRecord.DeleteRQL(TNullablesTest, 'eq(f_int2,2)');
  finally
    lTest.Free;
  end;

  Assert(TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = 2', [], False) = nil);

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

  Log('There are ' + TMVCActiveRecord.Count<TCustomer>().ToString + ' row/s for entity ' +
    TCustomer.ClassName);
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

procedure TMainForm.btnPartitioningClick(Sender: TObject);
var
  lCust1: TCustomerWithRate1;
  lCust2: TCustomerWithRate2;
  lList: TObjectList<TCustomerWithRate1>;
begin
  Log('** Partitioning Test');
  TMVCActiveRecord.DeleteAll(TCustomerWithRate1);
  Assert(TMVCActiveRecord.Count(TCustomerWithRate1) = 0);
  TMVCActiveRecord.DeleteAll(TCustomerWithRate2);
  Assert(TMVCActiveRecord.Count(TCustomerWithRate2) = 0);
  lCust1 := TCustomerWithRate1.Create;
  try
    lCust1.City := 'Rome';
    lCust1.Code := '123';
    lCust1.Store;
  finally
    lCust1.Free;
  end;
  lCust2 := TCustomerWithRate2.Create;
  try
    lCust2.City := 'Rome';
    lCust2.Code := '456';
    lCust2.Store;
    Assert(TMVCActiveRecord.GetByPK<TCustomerWithRate1>(lCust2.ID, False) = nil);
  finally
    lCust2.Free;
  end;

  lList := TMVCActiveRecord.SelectRQL<TCustomerWithRate1>('eq(city,"Rome")',-1);
  try
    Assert(lList.Count = 1);
    Assert(lList[0].Code = '123');
  finally
    lList.Free;
  end;

  Log('Retriving only TCustomerWithRate1');
  Assert(TMVCActiveRecord.Count(TCustomerWithRate1) = 1);
  Assert(TMVCActiveRecord.Count(TCustomerWithRate1, 'eq(code,"xxx")') = 0);
  Log('Retriving only TCustomerWithRate2');
  Assert(TMVCActiveRecord.Count(TCustomerWithRate2) = 1);
  Assert(TMVCActiveRecord.Count(TCustomerWithRate2, 'eq(code,"xxx")') = 0);
end;

procedure TMainForm.btnReadAndWriteOnlyClick(Sender: TObject);
var
  lArtWO, lArtWO2: TArticleWithWriteOnlyFields;
  lArtRO: TArticleWithReadOnlyFields;
  lID: NullableInt32;
  lArt: TArticle;
begin
  lArtWO := TArticleWithWriteOnlyFields.Create();
  try
    lArtWO.Description := 'Description1';
    lArtWO.Price := 12;
    lArtWO.Insert;
    Log('Stored TArticleWithWriteOnlyFields');
    lID := lArtWO.ID;

    lArt := TMVCActiveRecord.GetByPK<TArticle>(lID);
    try
      Assert(lArtWO.Description = lArt.Description);
      Assert(lArtWO.Price = lArt.Price);
      Log('Check Stored version of TArticleWithWriteOnlyFields');

      Log('Reading data using TArticleWithReadOnlyFields');
      lArtRO := TMVCActiveRecord.GetByPK<TArticleWithReadOnlyFields>(lID);
      try
        Assert(lArtRO.Description = lArt.Description);
        Assert(lArtRO.Price = lArt.Price);
        Log('Check Read data of TArticleWithWriteOnlyFields using TArticleWithReadOnlyFields');
      finally
        lArtRO.Free;
      end;

      Log('Reading data using TArticleWithWriteOnlyFields (???)');
      lArtWO2 := TMVCActiveRecord.GetByPK<TArticleWithWriteOnlyFields>(lID);
      try
        Assert(lArtWO2.ID.ValueOrDefault = lID.ValueOrDefault);
        Assert(lArtWO2.Description = '');
        Assert(lArtWO2.Price = 0);
      finally
        lArtWO2.Free;
      end;
    finally
      lArt.Free;
    end;

    lArtRO := TArticleWithReadOnlyFields.Create();
    try
      lArtRO.Description := 'Description1';
      lArtRO.Price := 12;
      ShowMessage('Now an exception will be raised...');
      lArtRO.Insert; // exception here :-)
    finally
      lArtRO.Free;
    end;

  finally
    lArtWO.Free;
  end;
end;

procedure TMainForm.btnReadOnlyClick(Sender: TObject);
begin
  var lROCustomer := TMVCActiveRecord.GetFirstByWhere<TReadOnlyCustomer>('',[]);
  try
    lROCustomer.Code := '1234';
    ShowMessage('An exception is going to be raised');
    lROCustomer.Update();
  finally
    lROCustomer.Free;
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
          Log(Format('         %-20s - %4d - %m', [lOrderRow.Description, lOrderRow.Quantity,
            lOrder.Total]));
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
  lRecCount: Integer;
const
  cRQL1 = 'in(City,["Rome","London"]);sort(+code);limit(0,50)';
  cRQL2 = 'and(eq(City,"Rome"),or(contains(CompanyName,"GAS"),contains(CompanyName,"Motors")))';
begin
  LoadCustomers;

  Log('** RQL Queries Test');
  Log('>> RQL Query (1) - ' + cRQL1);
  lList := TMVCActiveRecord.SelectRQL(TCustomer, cRQL1, 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
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
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
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
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

  Log('**RQL Query (4) - <empty> with limit 20');
  lList := TMVCActiveRecord.SelectRQL(TCustomer, '', 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 20);
  finally
    lList.Free;
  end;

  Log('**RQL Query (5) - <empty> sort by code with limit 20');
  lList := TMVCActiveRecord.SelectRQL(TCustomer, 'sort(+code)', 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 20);
  finally
    lList.Free;
  end;

  Log('**RQL Query (6) - <empty> with limit 10');
  lList := TMVCActiveRecord.SelectRQL(TCustomer, '', 10);
  try
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 10);
  finally
    lList.Free;
  end;

  Log('**RQL Query (7) - <empty> with limit 1');
  lList := TMVCActiveRecord.SelectRQL(TCustomer, '', 1);
  try
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 1);
  finally
    lList.Free;
  end;

  Log('**RQL Query (8) - <empty> with limit 0');
  lList := TMVCActiveRecord.SelectRQL(TCustomer, '', 0);
  try
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 0);
  finally
    lList.Free;
  end;


  //******************************************************
  // Using "Load" methods ********************************
  //******************************************************
  Log('*************************************************');
  Log('** RQL Queries Test (using "Load" style methods)');
  Log('*************************************************');
  Log('>> RQL Query (1) - ' + cRQL1);
  lList := TMVCActiveRecordList.Create;
  try
    TMVCActiveRecord.SelectRQL(TCustomer, cRQL1, 20, lList);
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

  Log('>> RQL Query (2) - ' + cRQL2);
  lCustList := TObjectList<TCustomer>.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL<TCustomer>(cRQL2, 20, lCustList);
    Log(lRecCount.ToString + ' record/s found');
    for lCustomer in lCustList do
    begin
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
    end;
  finally
    lCustList.Free;
  end;

  Log('**RQL Query (3) - ' + cRQL2);
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, cRQL2, 20, lList);
    Log(lRecCount.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomer(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;

  Log('**RQL Query (4) - <empty> with limit 20');
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, '', 20, lList);
    Log(lRecCount.ToString + ' record/s found');
    Assert(lRecCount = 20);
    Assert(lList.Count = lRecCount);
  finally
    lList.Free;
  end;

  Log('**RQL Query (5) - <empty> sort by code with limit 20');
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, 'sort(+code)', 20, lList);
    Log(lRecCount.ToString + ' record/s found');
    Assert(lRecCount = lList.Count);
    Assert(lList.Count = 20);
  finally
    lList.Free;
  end;

  Log('**RQL Query (6) - <empty> with limit 10');
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, '', 10, lList);
    Log(lList.Count.ToString + ' record/s found');
    Assert(lRecCount = lList.Count);
    Assert(lList.Count = 10);
  finally
    lList.Free;
  end;

  Log('**RQL Query (7) - <empty> with limit 1');
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, '', 1, lList);
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 1);
    Assert(lRecCount = lList.Count);
  finally
    lList.Free;
  end;

  Log('**RQL Query (8) - <empty> with limit 0');
  lList := TMVCActiveRecordList.Create;
  try
    lRecCount := TMVCActiveRecord.SelectRQL(TCustomer, '', 0, lList);
    Log(lList.Count.ToString + ' record/s found');
    Assert(lList.Count = 0);
    Assert(lRecCount = lList.Count);
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
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description CONTAINING ?', ['google'])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'mysql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'postgresql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description ILIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'sqlite' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'interbase' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else if ActiveRecordConnectionsRegistry.GetCurrentBackend = 'mssql' then
    lCustomers := TMVCActiveRecord.Select<TCustomer>
      ('SELECT * FROM customers WHERE description LIKE ''%google%''', [])
  else
    raise Exception.Create('Unsupported backend: ' +
      ActiveRecordConnectionsRegistry.GetCurrentBackend);

  try
    for lCustomer in lCustomers do
    begin
      Log(Format('%4d - %8.5s - %s', [lCustomer.ID.ValueOrDefault, lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault]));
    end;
  finally
    lCustomers.Free;
  end;

  LoadCustomers;

  Log('** Query SQL returning DataSet');
  lDS := TMVCActiveRecord.SelectDataSet('SELECT * FROM customers', [], True);
  try
    while not lDS.Eof do
    begin
      Log(Format('%8.5s - %s', [lDS.FieldByName('code').AsString, lDS.FieldByName('description')
        .AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

  lDS := TMVCActiveRecord.SelectDataSet
    ('SELECT * FROM orders o join customers c on c.id = o.id_customer where o.order_date >= ?',
    [Date - 5000], [ftDate]);
  try
    while not lDS.Eof do
    begin
      Log(Format('OrderDate: %12s - Customer: %s',
        [datetostr(lDS.FieldByName('order_date').AsDateTime), lDS.FieldByName('description')
        .AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

  lDS := TMVCActiveRecord.SelectDataSet
    ('SELECT * FROM orders o left join customers c on c.id = o.id_customer where o.order_date >= ? and c.id > ?',
    [Date - 5000, 1], [ftDate]);
  try
    while not lDS.Eof do
    begin
      Log(Format('OrderDate: %12s - Customer: %s',
        [datetostr(lDS.FieldByName('order_date').AsDateTime), lDS.FieldByName('description')
        .AsString]));
      lDS.Next;
    end;
  finally
    lDS.Free;
  end;

  Log('** GetFirstByWhere');
  lCustomer := TMVCActiveRecord.GetFirstByWhere<TCustomer>('id > ?', [1]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault,
      lCustomer.CompanyName.ValueOrDefault]));
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetFirstByWhere<TCustomer>('id > ?', [1], [ftInteger]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault,
      lCustomer.CompanyName.ValueOrDefault]));
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  Log('** GetOneByWhere');
  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID.Value]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault,
      lCustomer.CompanyName.ValueOrDefault]));
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID.Value], [ftInteger]);
  try
    Log(Format('%8.5s - %s', [lCustomer.Code.ValueOrDefault,
      lCustomer.CompanyName.ValueOrDefault]));
  finally
    lCustomer.Free;
  end;

end;

procedure TMainForm.btnSpeedClick(Sender: TObject);
var
  I: Integer;
  lCustomers: TArray<TCustomer>;
  lSW: TStopWatch;
  lElapsedMS: UInt32;
const
  INSTANCES_COUNT = 2000000;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  SetLength(lCustomers, INSTANCES_COUNT);
  lSW := TStopwatch.StartNew;
  for I := 0 to INSTANCES_COUNT - 1 do
  begin
    lCustomers[I] := TCustomer.Create;
  end;
  lElapsedMS := lSW.ElapsedMilliseconds;
  Log(Format('Created %s TCustomer instances in %d ms',
    [FormatFloat('###,###,###', INSTANCES_COUNT), lElapsedMS]));
  for I := 0 to INSTANCES_COUNT - 1 do
  begin
    lCustomers[I].Free;
  end;
end;

procedure TMainForm.btnTableFilterClick(Sender: TObject);
var
  i: Integer;
  lIDOfABadCustomer: Int64;
  lIDOfAGoodCustomer: Int64;
  lHowMany: Int64;
  lCust: TMVCActiveRecord;
  Customer: TCustomer;
  lCustomer: TCustomer;
  lCustomer1: TCustomer;
  lNotAGoodCustomer: TCustomer;
  lThisShouldBeNil: TCustomer;
  lAGoodCustomer: TCustomer;
  lThisShouldNotBeNil: TCustomer;
  lGoodCustomers: TObjectList<TGoodCustomer>;
  lGoodCustomers2: TMVCActiveRecordList;
begin
  Log('**Table Filtering');
  Log('Deleting only best customers...');
  lIDOfABadCustomer := -1;
  lIDOfAGoodCustomer := -1;
  TMVCActiveRecord.DeleteAll(TGoodCustomer);
  Log('Inserting some customers');
  for i := 1 to 5 do
  begin
    Customer := TCustomer.Create();
    try
      Customer.Code := I.ToString;
      Customer.Rating := I;
      Customer.Store;
      if i = 1 then
      begin
        lIDOfABadCustomer := Customer.ID.Value;
      end;
      if i = 5 then
      begin
        lIDOfAGoodCustomer := Customer.ID.Value;
      end;
    finally
      Customer.Free;
    end;
  end;

  Log('Retrieving only best customers...');
  lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('sort(+rating)',10);
  try
    Assert(lGoodCustomers.Count = 2); { only rating >= 4}
    for lCust in lGoodCustomers do
    begin
      Log(lCust.ToString);
    end;
  finally
    lGoodCustomers.Free;
  end;

  Log('How many "best customers" we have?');
  lHowMany := TMVCActiveRecord.Count<TGoodCustomer>;
  Log(Format('We have %d best customers', [lHowMany]));

  Log('How many "best customers" with rating = 5 we have?');
  lHowMany := TMVCActiveRecord.Count<TGoodCustomer>('eq(rating,5)');
  Log(Format('We have %d best customers with rating = 5', [lHowMany]));

  Log('Retrieving only best customers...');
  lGoodCustomers2 := TMVCActiveRecord.SelectRQL(TGoodCustomer, '', -1);
  try
    Assert(lGoodCustomers2.Count = 2); { only rating >= 4}
    for lCust in lGoodCustomers2 do
    begin
      Log(lCust.ToString);
    end;
  finally
    lGoodCustomers2.Free;
  end;

  Log('Retrieving only best customers ordered by company name...');
  lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('sort(+CompanyName)',10);
  try
    Assert(lGoodCustomers.Count = 2); { only rating >= 4}
    for lCust in lGoodCustomers do
    begin
      Log(lCust.ToString);
    end;
  finally
    lGoodCustomers.Free;
  end;


  Log('Retrieving only worst customers...');

  lNotAGoodCustomer := TMVCActiveRecord.SelectOneByRQL<TCustomer>('eq(rating,1)', True);
  try
    lThisShouldBeNil := TMVCActiveRecord.GetByPK<TGoodCustomer>(lNotAGoodCustomer.ID, False);
    Assert(lThisShouldBeNil = nil);
  finally
    lNotAGoodCustomer.Free;
  end;

  lAGoodCustomer := TMVCActiveRecord.SelectOneByRQL<TCustomer>('eq(rating,5)', True);
  try
    lThisShouldNotBeNil := TMVCActiveRecord.GetByPK<TGoodCustomer>(lAGoodCustomer.ID, False);
    try
      Assert(lThisShouldNotBeNil <> nil);
      Log(lThisShouldNotBeNil.ToString);
    finally
      lThisShouldNotBeNil.Free;
    end;
  finally
    lAGoodCustomer.Free;
  end;

  Log('Promoting a customer...');
  lCustomer := TBadCustomer.Create;
  try
    lCustomer.LoadByPK(lIDOfABadCustomer);
    lCustomer.Rating := 5;
    lCustomer.Store;
    Assert(not lCustomer.LoadByPK(lIDOfABadCustomer)); {this customer is not "bad" anymore}
  finally
    lCustomer.Free;
  end;

  Log('Demote a customer...');
  lCustomer1 := TGoodCustomer.Create;
  try
    lCustomer1.LoadByPK(lIDOfAGoodCustomer);
    lCustomer1.Rating := 1;
    lCustomer1.Store;
    Assert(not lCustomer1.LoadByPK(lIDOfAGoodCustomer)); {this customer is not "good" anymore}
  finally
    lCustomer1.Free;
  end;
end;

procedure TMainForm.btnReadOnlyFieldsClick(Sender: TObject);
var
  lCustomer: TCustomerWithReadOnlyFields;
  lID: Integer;
begin
  Log('** CRUD test with read-only fields');
  lCustomer := TCustomerWithReadOnlyFields.Create;
  try
    {
      'Code' will not be persisted on table because defined as 'foReadOnly'
    }
    lCustomer.Code := '1234';
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Insert;
    lID := lCustomer.ID;
    Log('Just inserted Customer ' + lID.ToString + ' with a R/O field');
  finally
    lCustomer.Free;
  end;

  //let's check that code is empty
  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithReadOnlyFields>(lID);
  try
    Assert(lCustomer.Code.IsEmpty);
  finally
    lCustomer.Free;
  end;

  //if underlying field is not null, it is loaded as usual
  TMVCActiveRecord.CurrentConnection.ExecSQL('update customers set code = ''XYZ'' where id = ?', [lID]);
  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithReadOnlyFields>(lID);
  try
    Assert('XYZ' = lCustomer.Code);
    lCustomer.CompanyName := lCustomer.CompanyName + ' changed!';
    lCustomer.Code := 'this code will not be saved';
    lCustomer.Update; //do not save field "code"
    Log('Just updated Customer ' + lID.ToString);
  finally
    lCustomer.Free;
  end;

  //but being foReadOnly is not updated
  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithReadOnlyFields>(lID);
  try
    Assert('XYZ' = lCustomer.Code);
    lCustomer.Delete;
    Log('Just deleted Customer ' + lID.ToString + ' with a R/O field');
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Log('** Refresh test');
  lCustomer := TCustomer.Create;
  try
    Log('Entity ' + TCustomer.ClassName + ' is mapped to table ' + lCustomer.TableName);
    lCustomer.CompanyName := 'Google Inc.';
    lCustomer.City := 'Montain View, CA';
    lCustomer.Note := 'Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁';
    lCustomer.Insert;
    Assert('Montain View, CA' = lCustomer.City);
    Assert('Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁' = lCustomer.Note);
    lCustomer.City := '';
    lCustomer.Note := '';
    Log('Refreshing the customer');
    lCustomer.Refresh;
    Assert('Montain View, CA' = lCustomer.City);
    Assert('Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁' = lCustomer.Note);
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    Log('Loading customer using Refresh');
    lCustomer.ID := lID;
    lCustomer.Refresh;
    Assert('Montain View, CA' = lCustomer.City);
    Assert('Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην 😁' = lCustomer.Note);
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

  ShowMessage('Try to update a customer with empty "CODE" (an exception will be raised)');

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithLogic>(lID);
  try
    Log(lCustomer.CompanyName + ' => IsLocatedInRome: ' +
      BoolToStr(lCustomer.IsLocatedInRome, True));
    lCustomer.Code := '';
    lCustomer.Update; // raise exception
  finally
    lCustomer.Free;
  end;
end;

procedure TMainForm.btnVirtualEntitiesClick(Sender: TObject);
begin
  var lCustStats := TMVCActiveRecord.SelectByNamedQuery<TCustomerStats>('CustomersInTheSameCity', [], []);
  try
    for var lCustomer in lCustStats do
    begin
      Log(Format('%4d - %8.5s - %s - (%d other customers in the same city)', [
        lCustomer.ID.ValueOrDefault,
        lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault,
        lCustomer.CustomersInTheSameCity
        ]));
    end;
  finally
    lCustStats.Free;
  end;
end;

procedure TMainForm.btnWithSpacesClick(Sender: TObject);
var
  lCustomer: TCustomerWithSpaces;
  lID: Integer;
  I: Integer;
  cRQL1: string;
  lList: TMVCActiveRecordList;
  lItem: TMVCActiveRecord;
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
      lID := I;
      lCustomer.ID := lID;
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
      Log('Just inserted Customer ' + lID.ToString);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerWithSpaces>().ToString +
    ' row/s for entity ' + TCustomerWithSpaces.ClassName);
  Log('Deleting using RQL...');
  TMVCActiveRecord.DeleteRQL(TCustomerWithSpaces, 'lt(id,80)');
  Log('Now there are ' + TMVCActiveRecord.Count<TCustomerWithSpaces>().ToString +
    ' row/s for entity ' + TCustomerWithSpaces.ClassName);

  // gets the last inserted customer
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

  cRQL1 := 'eq(CompanyName,"Google Inc.")';
  Log('>> RQL Query (customers with spaces) - ' + cRQL1);
  lList := TMVCActiveRecord.SelectRQL(TCustomerWithSpaces, cRQL1, 20);
  try
    Log(lList.Count.ToString + ' record/s found');
    for lItem in lList do
    begin
      lCustomer := TCustomerWithSpaces(lItem);
      Log(Format('%5s - %s (%s)', [lCustomer.Code.ValueOrDefault,
        lCustomer.CompanyName.ValueOrDefault, lCustomer.City]));
    end;
  finally
    lList.Free;
  end;
end;

procedure TMainForm.btnOOPClick(Sender: TObject);
begin
  Log('** OOP with ActiveRecord (person, employee, manager)');
  TMVCActiveRecord.DeleteAll(TPerson);

  var lPerson := TPerson.Create;
  try
    lPerson.FirstName := 'Reed';
    lPerson.LastName := 'Richards';
    lPerson.Dob := EncodeDate(1985,11,4);
    lPerson.IsMale := True;
    lPerson.Store;
  finally
    lPerson.Free;
  end;

  var lEmployee := TEmployee.Create;
  try
    lEmployee.FirstName := 'Peter';
    lEmployee.LastName := 'Parker';
    lEmployee.Dob := EncodeDate(1985,11,4);
    lEmployee.IsMale := True;
    lEmployee.Salary := 2100;
    lEmployee.Store;
  finally
    lEmployee.Free;
  end;

  lEmployee := TEmployee.Create;
  try
    lEmployee.FirstName := 'Sue';
    lEmployee.LastName := 'Storm';
    lEmployee.Dob := EncodeDate(1975,10,14);
    lEmployee.IsMale := False;
    lEmployee.Salary := 2200;
    lEmployee.Store;
  finally
    lEmployee.Free;
  end;

  var lManager := TManager.Create;
  try
    lManager.FirstName := 'Bruce';
    lManager.LastName := 'Banner';
    lManager.Dob := EncodeDate(1975,11,4);
    lManager.IsMale := True;
    lManager.Salary := 2800;
    lManager.AnnualBonus := 5000;
    lManager.Store;
  finally
    lManager.Free;
  end;

  var lPeople := TMVCActiveRecord.All<TPerson>;
  try
    Assert(lPeople.Count = 4);
  finally
    lPeople.Free;
  end;

  var lEmployees := TMVCActiveRecord.All<TEmployee>;
  try
    Assert(lEmployees.Count = 3);
  finally
    lEmployees.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection(False);
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
  btnWithSpaces.Enabled := (ActiveRecordConnectionsRegistry.GetCurrentBackend = 'postgresql') or
    (ActiveRecordConnectionsRegistry.GetCurrentBackend = 'firebird') or
    (ActiveRecordConnectionsRegistry.GetCurrentBackend = 'interbase') or
    (ActiveRecordConnectionsRegistry.GetCurrentBackend = 'sqlite');

  btnJSON_XML_Types.Enabled := ActiveRecordConnectionsRegistry.GetCurrentBackend = 'postgresql';
end;

procedure TMainForm.LoadCustomers;
var
  lCustomer: TCustomer;
  I: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  for I := 1 to 50 do
  begin
    lCustomer := TCustomer.Create;
    try
      lCustomer.CompanyName := Stuff[Random(4)] + ' ' + CompanySuffix[Random(5)];
      lCustomer.Code := Random(100).ToString.PadLeft(5, '0');
      lCustomer.City := Cities[Random(4)];
      lCustomer.Rating := Random(5);
      lCustomer.Note := Stuff[Random(4)];
      lCustomer.Insert;
    finally
      lCustomer.Free;
    end;
  end;
end;

procedure TMainForm.Log(const Value: string);
begin
  Memo1.Lines.Add(Value);
  Memo1.Update;
end;


end.
