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
  BusinessObjectsU;

{$R *.dfm}

procedure TForm10.btnAddDayClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'getnextmonday';
    lReq.RequestID := Random(1000);
    lReq.Params.Add(dtNextMonday.Date);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      dtNextMonday.Date := ISOTimeStampToDateTime(lResp.Result.AsString);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnGetUserClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
  lJSON: TJsonObject;
begin
  lbPerson.Clear;
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'getuser';
    lReq.RequestID := Random(1000);
    lReq.Params.Add(edtUserName.Text);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      if Assigned(lResp.Error) then
        raise Exception.Create(lResp.Error.ErrMessage);

      // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
      // are serialized as JSON objects
      lJSON := lResp.Result.AsObject as TJsonObject;
      lbPerson.Items.Add('First Name:'.PadRight(15) + lJSON.S['firstname']);
      lbPerson.Items.Add('Last Name:'.PadRight(15) + lJSON.S['lastname']);
      lbPerson.Items.Add('Married:'.PadRight(15) + lJSON.B['married'].ToString(TUseBoolStrs.True));
      lbPerson.Items.Add('DOB:'.PadRight(15) + DateToStr(lJSON.D['dob']));
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnInvalid1Click(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'invalidmethod1';
    lReq.Params.Add(1);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      ShowMessage(lResp.Error.ErrMessage);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnInvalid2Click(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'invalidmethod2';
    lReq.Params.Add(1);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      ShowMessage(lResp.Error.ErrMessage);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;

end;

procedure TForm10.btnInvalidMethodClick(Sender: TObject);
var
  lNotification: TJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  try
    lNotification.Method := 'notexists';
    FExecutor.ExecuteNotification(lNotification);
  finally
    lNotification.Free;
  end;
end;

procedure TForm10.btnNotificationClick(Sender: TObject);
var
  lNotification: TJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  try
    lNotification.Method := 'dosomething';
    FExecutor.ExecuteNotification(lNotification);
  finally
    lNotification.Free;
  end;
end;

procedure TForm10.btnReverseStringClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'reversestring';
    lReq.RequestID := Random(1000);
    lReq.Params.Add(edtReverseString.Text);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      edtReversedString.Text := lResp.Result.AsString;
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnSaveClick(Sender: TObject);
var
  lPerson: TPerson;
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'saveperson';
    lReq.RequestID := Random(1000);
    lPerson := TPerson.Create;
    lReq.Params.Add(lPerson);
    lPerson.FirstName := edtFirstName.Text;
    lPerson.LastName := edtLastName.Text;
    lPerson.Married := chkMarried.Checked;
    lPerson.DOB := dtDOB.Date;
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      ShowMessage('Person saved with ID = ' + lResp.Result.AsInteger.ToString);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnSubstractClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'subtract';
    lReq.RequestID := Random(1000);
    lReq.Params.Add(StrToInt(edtValue1.Text));
    lReq.Params.Add(StrToInt(edtValue2.Text));
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      edtResult.Text := lResp.Result.AsInteger.ToString;
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.edtGetCustomersClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'getcustomers';
    lReq.RequestID := Random(1000);
    lReq.Params.Add(edtFilter.Text);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      FDMemTable1.Active := True;
      FDMemTable1.LoadFromTValue(lResp.Result);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080/jsonrpc');
  dtNextMonday.Date := Date;
end;

end.
