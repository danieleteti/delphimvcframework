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
  MVCFramework.JSONRPC.Client;

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
    GroupBox3: TGroupBox;
    edtFilter: TEdit;
    edtGetCustomers: TButton;
    DBGrid1: TDBGrid;
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
  private
    FExecutor: IMVCJSONRPCExecutor;
    FExecutor2: IMVCJSONRPCExecutor;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  MVCFramework.JSONRPC,
  MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
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
  lResp := FExecutor.ExecuteRequest(lReq);
  dtNextMonday.Date := ISOTimeStampToDateTime(lResp.Result.AsString);
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
  lResp := FExecutor.ExecuteRequest(lReq);
  ShowMessage(DateTimeToStr(lResp.Result.AsType<TDateTime>));
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
  lResp := FExecutor.ExecuteRequest(lReq);
  lRes := lResp.Result.AsType<Extended>;
  lRes := RoundTo(lRes, -4);
  Assert(SameValue(lRes, 3580.2467), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18,9));

  lReq := TJSONRPCRequest.Create(1234, 'floatstest');
  lReq.Params.Add(123);
  lReq.Params.Add(234);
  lResp := FExecutor.ExecuteRequest(lReq);
  lRes := lResp.Result.AsType<Extended>;
  lRes := RoundTo(lRes, -4);
  Assert(SameValue(lRes, 357), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18,9));
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
  lResp := FExecutor.ExecuteRequest(lReq);
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
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'invalidmethod1';
  lReq.Params.Add(1);
  lResp := FExecutor.ExecuteRequest(lReq);
  ShowMessage(lResp.Error.ErrMessage);
end;

procedure TMainForm.btnInvalid2Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'invalidmethod2';
  lReq.Params.Add(1);
  lResp := FExecutor.ExecuteRequest(lReq);
  ShowMessage(lResp.Error.ErrMessage);
end;

procedure TMainForm.btnInvalidMethodClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'notexists';
  FExecutor.ExecuteNotification(lNotification);
end;

procedure TMainForm.btnNotificationClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'dosomething';
  FExecutor.ExecuteNotification(lNotification);
end;

procedure TMainForm.btnReverseStringClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'reversestring';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(edtReverseString.Text);
  lReq.Params.Add(CheckBox1.Checked);
  lResp := FExecutor.ExecuteRequest(lReq);
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
  lReq.Params.Add(lPerson, pdtObject);
  lPerson.FirstName := edtFirstName.Text;
  lPerson.LastName := edtLastName.Text;
  lPerson.Married := chkMarried.Checked;
  lPerson.DOB := dtDOB.Date;
  lResp := FExecutor.ExecuteRequest(lReq);
  ShowMessage('Person saved with ID = ' + lResp.Result.AsInteger.ToString);
end;

procedure TMainForm.btnSearchClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lJSON: TJsonArray;
  I: Integer;
begin
  lbPerson.Clear;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'searchproducts';
  lReq.RequestID := 1234;
  lReq.Params.Add(edtSearchText.Text);
  lResp := FExecutor2.ExecuteRequest(lReq);
  if Assigned(lResp.Error) then
    raise Exception.Create(lResp.Error.ErrMessage);

  // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
  // are serialized as JSON objects
  lJSON := lResp.Result.AsObject as TJsonArray;
  for I := 0 to lJSON.Count - 1 do
  begin
    ListBox1.Items.Add(lJSON[I].ObjectValue.ToJSON());
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
  lResp := FExecutor.ExecuteRequest(lReq);
  edtResult.Text := lResp.Result.AsInteger.ToString;
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
  lResp := FExecutor.ExecuteRequest(lReq);

  lPerson := lResp.Result.AsObject as TJsonObject;
  ShowMessage(lPerson.ToJSON(False));
end;

procedure TMainForm.edtGetCustomersClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getcustomers');
  lReq.Params.Add(edtFilter.Text);
  lResp := FExecutor.ExecuteRequest(lReq);
  FDMemTable1.Active := True;
  FDMemTable1.LoadFromTValue(lResp.Result);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080/jsonrpc');
  FExecutor2 := TMVCJSONRPCExecutor.Create('http://localhost:8080/rpcdatamodule');
  dtNextMonday.Date := Date;

  // these are the methods to handle http headers in JSONRPC
  // the following line and the check on the server is just for demo
  assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
  assert(FExecutor.HTTPHeadersCount = 1);
  FExecutor.ClearHTTPHeaders;
  assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
end;

end.
