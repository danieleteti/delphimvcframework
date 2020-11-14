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
  TForm10 = class(TForm)
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
  private
    FExecutor: IMVCJSONRPCExecutor;
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses
  MVCFramework.JSONRPC,
  MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.DataSet.Utils,
  BusinessObjectsU, MVCFramework.Commons;

{$R *.dfm}


procedure TForm10.btnAddDayClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getnextmonday';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(dtNextMonday.Date);
  lResp := FExecutor.ExecuteRequest(lReq);
  dtNextMonday.Date := ISODateToDate(lResp.Result.AsString);
end;

procedure TForm10.btnGetUserClick(Sender: TObject);
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

procedure TForm10.btnInvalid1Click(Sender: TObject);
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

procedure TForm10.btnInvalid2Click(Sender: TObject);
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

procedure TForm10.btnInvalidMethodClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'notexists';
  FExecutor.ExecuteNotification(lNotification);
end;

procedure TForm10.btnNotificationClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'dosomething';
  FExecutor.ExecuteNotification(lNotification);
end;

procedure TForm10.btnReverseStringClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'reversestring';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(edtReverseString.Text);
  lResp := FExecutor.ExecuteRequest(lReq);
  edtReversedString.Text := lResp.Result.AsString;
end;

procedure TForm10.btnSaveClick(Sender: TObject);
var
  lPerson: TPerson;
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'saveperson';
  lReq.RequestID := Random(1000);
  lPerson := TPerson.Create;
  lReq.Params.Add(lPerson, TJSONRPCParamDataType.pdtObject);
  lPerson.FirstName := edtFirstName.Text;
  lPerson.LastName := edtLastName.Text;
  lPerson.Married := chkMarried.Checked;
  lPerson.DOB := dtDOB.Date;
  lResp := FExecutor.ExecuteRequest(lReq);
  ShowMessage('Person saved with ID = ' + lResp.Result.AsInteger.ToString);
end;

procedure TForm10.btnSubstractClick(Sender: TObject);
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

procedure TForm10.edtGetCustomersClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getcustomers';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(edtFilter.Text);
  lResp := FExecutor.ExecuteRequest(lReq);
  FDMemTable1.Active := True;
  FDMemTable1.LoadFromTValue(lResp.Result);
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  Caption := 'JSON-RPC 2.0 Sample - DMVCFramework ' + DMVCFRAMEWORK_VERSION;
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080/jsonrpc');
  dtNextMonday.Date := Date;

  // these are the methods to handle http headers in JSONRPC
  // the following line and the check on the server is just for demo
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
  Assert(FExecutor.HTTPHeadersCount = 1);
  FExecutor.ClearHTTPHeaders;
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
end;

end.
