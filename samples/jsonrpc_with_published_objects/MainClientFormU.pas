unit MainClientFormU;

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
  System.Net.HttpClientComponent,
  Vcl.StdCtrls,
  System.Net.URLClient,
  System.Net.HttpClient,
  Data.DB,
  Vcl.Grids,
  Vcl.DBGrids,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  MVCFramework.JSONRPC.Client, Vcl.Mask;

type
  TMainForm = class(TForm)
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    FDMemTable1Code: TIntegerField;
    FDMemTable1Name: TStringField;
    GroupBox1: TGroupBox;
    edtValue1: TEdit;
    edtValue2: TEdit;
    btnSubstract: TButton;
    edtResult: TEdit;
    edtReverseString: TEdit;
    btnReverseString: TButton;
    edtReversedString: TEdit;
    GroupBox2: TGroupBox;
    edtUserName: TEdit;
    btnGetUser: TButton;
    lbPerson: TListBox;
    GroupBox4: TGroupBox;
    edtFirstName: TLabeledEdit;
    edtLastName: TLabeledEdit;
    chkMarried: TCheckBox;
    dtDOB: TDateTimePicker;
    btnSave: TButton;
    dtNextMonday: TDateTimePicker;
    btnAddDay: TButton;
    btnInvalid1: TButton;
    btnInvalid2: TButton;
    btnNotification: TButton;
    btnInvalidMethod: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox5: TGroupBox;
    edtSearchText: TEdit;
    btnSearch: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    btnDates: TButton;
    btnFloatsTests: TButton;
    btnWithJSON: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    btnSubtractWithNamedParams: TButton;
    Edit3: TEdit;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    edtFilter: TEdit;
    edtGetCustomers: TButton;
    DBGrid1: TDBGrid;
    btnGetMulti: TButton;
    lbMulti: TListBox;
    btnGenericException: TButton;
    TabSheet5: TTabSheet;
    Label1: TLabel;
    btnException: TButton;
    btnGenericExcWithCustomHandling: TButton;
    btnGenericExcWithCustomHAndling2: TButton;
    btnGenericExcWithoutCustomHandling: TButton;
    procedure btnSubstractClick(Sender: TObject);
    procedure btnReverseStringClick(Sender: TObject);
    procedure edtGetCustomersClick(Sender: TObject);
    procedure btnGetUserClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddDayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInvalid1Click(Sender: TObject);
    procedure btnInvalid2Click(Sender: TObject);
    procedure btnNotificationClick(Sender: TObject);
    procedure btnInvalidMethodClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnDatesClick(Sender: TObject);
    procedure btnFloatsTestsClick(Sender: TObject);
    procedure btnWithJSONClick(Sender: TObject);
    procedure btnSubtractWithNamedParamsClick(Sender: TObject);
    procedure btnGetMultiClick(Sender: TObject);
    procedure btnGetListOfDatasetClick(Sender: TObject);
    procedure btnObjDictClick(Sender: TObject);
    procedure btnExceptionClick(Sender: TObject);
    procedure btnGenericExceptionClick(Sender: TObject);
    procedure btnGenericExcWithCustomHandlingClick(Sender: TObject);
    procedure btnGenericExcWithCustomHAndling2Click(Sender: TObject);
    procedure btnGenericExcWithoutCustomHandlingClick(Sender: TObject);
  private
    FExecutor: IMVCJSONRPCExecutor;
    // FExecutor2: IMVCJSONRPCExecutor;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Generics.Collections,
  MVCFramework.JSONRPC,
  MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Serializer.Defaults,
  MVCFramework.DataSet.Utils,
  BusinessObjectsU,
  System.Math,
  System.Rtti;

{$R *.dfm}

procedure TMainForm.btnAddDayClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getnextmonday';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(dtNextMonday.Date);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  dtNextMonday.Date := ISODateToDate(lResp.Result.AsString);
end;

procedure TMainForm.btnDatesClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create(1234, 'playwithdatesandtimes');
  lReq.Params.Add(1234.5678, pdtFloat);
  lReq.Params.Add(Time(), pdtTime);
  lReq.Params.Add(Date(), pdtDate);
  lReq.Params.Add(Now(), pdtDateTime);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  ShowMessage(lResp.Result.AsString);
end;

procedure TMainForm.btnExceptionClick(Sender: TObject);
var
  lReq: IJSONRPCNotification;
begin
  ShowMessage('Now will be raised a custom exception on the server. This exception will be catched by the client');
  lReq := TJSONRPCNotification.Create('RaiseCustomException');
  FExecutor.ExecuteNotification('/jsonrpc', lReq);
end;

procedure TMainForm.btnFloatsTestsClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lRes: Extended;
begin
  lReq := TJSONRPCRequest.Create(1234, 'floatstest');
  lReq.Params.Add(1234.5678, pdtFloat);
  lReq.Params.Add(2345.6789, pdtFloat);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  lRes := lResp.Result.AsType<Extended>;
  lRes := RoundTo(lRes, -4);
  Assert(SameValue(lRes, 3580.2467), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18, 9));

  lReq := TJSONRPCRequest.Create(1234, 'floatstest');
  lReq.Params.Add(123);
  lReq.Params.Add(234);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  lRes := lResp.Result.AsType<Extended>;
  lRes := RoundTo(lRes, -4);
  Assert(SameValue(lRes, 357), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18, 9));
end;

procedure TMainForm.btnGetUserClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lJSON: TJsonObject;
begin
  lbPerson.Clear;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getuser';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(edtUserName.Text);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  if Assigned(lResp.Error) then
    raise Exception.Create(lResp.Error.ErrMessage);

  // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
  // are serialized as JSON objects
  lJSON := lResp.Result.AsObject as TJsonObject;
  lbPerson.Items.Add('First Name:'.PadRight(15) + lJSON.S['firstname']);
  lbPerson.Items.Add('Last Name:'.PadRight(15) + lJSON.S['lastname']);
  lbPerson.Items.Add('Married:'.PadRight(15) + lJSON.B['married'].ToString(TUseBoolStrs.True));
  lbPerson.Items.Add('DOB:'.PadRight(15) + DateToStr(lJSON.D['dob']));
end;

procedure TMainForm.btnInvalid1Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create(1234);
  lReq.Method := 'invalidmethod1';
  lReq.Params.Add(1);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  ShowMessage(lResp.Error.ErrMessage);
end;

procedure TMainForm.btnInvalid2Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create(1234);
  lReq.Method := 'invalidmethod2';
  lReq.Params.Add(1);
  lResp := FExecutor.ExecuteNotification('/jsonrpc', lReq);
  ShowMessage(lResp.Error.ErrMessage);
end;

procedure TMainForm.btnInvalidMethodClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'notexists';
  FExecutor.ExecuteNotification('/jsonrpc', lNotification);
end;

procedure TMainForm.btnNotificationClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'dosomething';
  FExecutor.ExecuteNotification('/jsonrpc', lNotification);
end;

procedure TMainForm.btnObjDictClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lMultiDS: TMultiDataset;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getobjdict');
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);

  lMultiDS := TMultiDataset.Create;
  try
    JsonObjectToObject(lResp.ResultAsJSONObject, lMultiDS);
    lbMulti.Clear;

    lMultiDS.Customers.First;
    lbMulti.Items.Add('** CUSTOMERS **');
    while not lMultiDS.Customers.Eof do
    begin
      lbMulti.Items.Add(Format('%-20s (Code %3s)', [lMultiDS.Customers.FieldByName('Name').AsString,
        lMultiDS.Customers.FieldByName('Code').AsString]));
      lMultiDS.Customers.Next;
    end;

    lMultiDS.People.First;
    lbMulti.Items.Add('** PEOPLE **');
    while not lMultiDS.People.Eof do
    begin
      lbMulti.Items.Add(Format('%s %s', [lMultiDS.People.FieldByName('FirstName').AsString,
        lMultiDS.People.FieldByName('LastName').AsString]));
      lMultiDS.People.Next;
    end;

  finally
    lMultiDS.Free;
  end;
end;

procedure TMainForm.btnReverseStringClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'reversestring';
  lReq.RequestID := Random(1000);
  lReq.Params.AddByName('aString', edtReverseString.Text);
  lReq.Params.AddByName('aUpperCase', CheckBox1.Checked);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  edtReversedString.Text := lResp.Result.AsString;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  lPerson: TPerson;
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'saveperson';
  lReq.RequestID := Random(1000);
  lPerson := TPerson.Create;
  lReq.Params.AddByName('Person', lPerson, pdtObject);
  lPerson.FirstName := edtFirstName.Text;
  lPerson.LastName := edtLastName.Text;
  lPerson.Married := chkMarried.Checked;
  lPerson.DOB := dtDOB.Date;
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  ShowMessage('Person saved with ID = ' + lResp.Result.AsInteger.ToString);
end;

procedure TMainForm.btnSearchClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lJSON: TJsonArray;
  I: Integer;
  lJObj: TJsonObject;
begin
  ListBox1.Clear;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'searchproducts';
  lReq.RequestID := 1234;
  lReq.Params.Add(edtSearchText.Text);
  lResp := FExecutor.ExecuteRequest('/rpcdatamodule', lReq);
  if Assigned(lResp.Error) then
    raise Exception.Create(lResp.Error.ErrMessage);

  // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
  // are serialized as JSON objects
  lJSON := lResp.Result.AsObject as TJsonArray;
  for I := 0 to lJSON.Count - 1 do
  begin
    lJObj := lJSON[I].ObjectValue;
    ListBox1.Items.Add(Format('%6s: %-34s € %5.2f', [lJObj.S['codice'], lJObj.S['descrizione'], lJObj.F['prezzo']]));
  end;
  // lbPerson.Items.Add('First Name:'.PadRight(15) + lJSON.S['firstname']);
  // lbPerson.Items.Add('Last Name:'.PadRight(15) + lJSON.S['lastname']);
  // lbPerson.Items.Add('Married:'.PadRight(15) + lJSON.B['married'].ToString(TUseBoolStrs.True));
  // lbPerson.Items.Add('DOB:'.PadRight(15) + DateToStr(lJSON.D['dob']));
end;

procedure TMainForm.btnSubstractClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(StrToInt(edtValue1.Text));
  lReq.Params.Add(StrToInt(edtValue2.Text));
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  edtResult.Text := lResp.Result.AsInteger.ToString;
end;

procedure TMainForm.btnSubtractWithNamedParamsClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.RequestID := Random(1000);
  lReq.Params.AddByName('Value1', StrToInt(Edit1.Text));
  lReq.Params.AddByName('Value2', StrToInt(Edit2.Text));
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);
  Edit3.Text := lResp.Result.AsInteger.ToString;
end;

procedure TMainForm.btnWithJSONClick(Sender: TObject);
var
  lPerson: TJsonObject;
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'SaveObjectWithJSON';
  lReq.RequestID := 1234;
  lPerson := TJsonObject.Create;
  lReq.Params.Add(lPerson, pdTJDOJsonObject);
  lPerson.S['StringProp'] := 'Hello World';
  lPerson.O['JSONObject'] := TJsonObject.Parse('{"name":"Daniele"}') as TJsonObject;
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);

  lPerson := lResp.Result.AsObject as TJsonObject;
  ShowMessage(lPerson.ToJSON(False));
end;

procedure TMainForm.btnGenericExceptionClick(Sender: TObject);
var
  lReq: IJSONRPCNotification;
begin
  ShowMessage('Now will be raised a EDivByZero exception on the server. This exception will be catched by the client');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  FExecutor.ExecuteNotification('/jsonrpc', lReq);
end;

procedure TMainForm.btnGenericExcWithCustomHAndling2Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage('Now will be raised a EInvalidPointerOperation exception on the server. However this exception will be handled by a custom exception handler wich will add a data property with extra data');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(2);
  try
    FExecutor.ExecuteRequest('/jsonrpcex', lReq);
  except
    on E: EMVCJSONRPCRemoteException do
    begin
      ShowMessage(Format('[CLASSNAME: %s][CODE: %d][MESSAGE: %s][DATA: %s]', [
        E.ClassName,
        E.ErrCode,
        E.ErrMessage,
        (E.Data.AsObject as TJDOJsonObject).ToJSON(True)]));
    end;
  end;
end;

procedure TMainForm.btnGenericExcWithCustomHandlingClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage('Now will be raised a EDivByZero exception on the server. However this exception will be handled by a custom exception handler wich will add a data property with extra data');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(1);
  try
    FExecutor.ExecuteRequest('/jsonrpcex', lReq);
  except
    on E: EMVCJSONRPCRemoteException do
    begin
      ShowMessage(Format('[CLASSNAME: %s][CODE: %d][MESSAGE: %s][DATA: %s]', [
        E.ClassName,
        E.ErrCode,
        E.ErrMessage,
        E.Data.AsString]));
    end;
  end;
end;

procedure TMainForm.btnGenericExcWithoutCustomHandlingClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage('Now will be raised a Exception exception on the server.');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(99);
  try
    FExecutor.ExecuteRequest('/jsonrpcex', lReq);
  except
    on E: EMVCJSONRPCRemoteException do
    begin
      ShowMessage(Format('[CLASSNAME: %s][CODE: %d][MESSAGE: %s][DATA: %s]', [
        E.ClassName,
        E.ErrCode,
        E.ErrMessage,
        E.Data.AsString])); {Data.AsString is ''}
    end;
  end;
end;

procedure TMainForm.btnGetListOfDatasetClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lMultiDS: TObjectList<TDataSet>;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'GetDataSetList');
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq);

  lMultiDS := TObjectList<TDataSet>.Create(True);
  try
    JsonArrayToList(lResp.ResultAsJSONArray, WrapAsList(lMultiDS), TDataSet, TMVCSerializationType.stDefault, nil);
  finally
    lMultiDS.Free;
  end;
end;

procedure TMainForm.btnGetMultiClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lMultiDS: TMultiDataset;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getmulti');
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq, jrpcGET);

  lMultiDS := TMultiDataset.Create;
  try
    JsonObjectToObject(lResp.ResultAsJSONObject, lMultiDS);
    lbMulti.Clear;

    lMultiDS.Customers.First;
    lbMulti.Items.Add('** CUSTOMERS **');
    while not lMultiDS.Customers.Eof do
    begin
      lbMulti.Items.Add(Format('%-20s (Code %3s)', [lMultiDS.Customers.FieldByName('Name').AsString,
        lMultiDS.Customers.FieldByName('Code').AsString]));
      lMultiDS.Customers.Next;
    end;

    lMultiDS.People.First;
    lbMulti.Items.Add('** PEOPLE **');
    while not lMultiDS.People.Eof do
    begin
      lbMulti.Items.Add(Format('%s %s', [lMultiDS.People.FieldByName('FirstName').AsString,
        lMultiDS.People.FieldByName('LastName').AsString]));
      lMultiDS.People.Next;
    end;

  finally
    lMultiDS.Free;
  end;
end;

procedure TMainForm.edtGetCustomersClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getcustomers');
  lReq.Params.AddByName('FilterString', edtFilter.Text);
  lResp := FExecutor.ExecuteRequest('/jsonrpc', lReq, jrpcGET);
  FDMemTable1.Active := True;
  FDMemTable1.LoadFromTValue(lResp.Result);
  FDMemTable1.First;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080');

  FExecutor.SetOnSendCommand(
    procedure(JSONRPCObject: IJSONRPCObject)
    begin
      Log.Debug('REQUEST : ' + JSONRPCObject.ToString(True), 'jsonrpc');
    end);

  FExecutor.SetOnReceiveResponse(
    procedure(Req, Resp: IJSONRPCObject)
    begin
      Log.Debug('>> OnReceiveResponse // start', 'jsonrpc');
      Log.Debug('     REQUEST : ' + Req.ToString(True), 'jsonrpc');
      Log.Debug('     RESPONSE: ' + Resp.ToString(True), 'jsonrpc');
      Log.Debug('<< OnReceiveResponse // end', 'jsonrpc');
    end);

  FExecutor.SetOnReceiveHTTPResponse(
    procedure(HTTPResp: IHTTPResponse)
    begin
      Log.Debug('RESPONSE: ' + HTTPResp.ContentAsString(), 'jsonrpc');
    end);

  dtNextMonday.Date := Date;
  // these are the methods to handle http headers in JSONRPC
  // the following line and the check on the server is just for demo
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
  Assert(FExecutor.HTTPHeadersCount = 1);
  FExecutor.ClearHTTPHeaders;
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));

  PageControl1.ActivePageIndex := 0;
end;

end.
