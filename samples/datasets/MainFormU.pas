unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, MVCFramework.DataSet.Utils, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, MVCFramework.ActiveRecord,
  REST.Response.Adapter, Vcl.Grids, Vcl.DBGrids;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    btnSaveDataSet: TButton;
    Connection: TFDConnection;
    CustomerTable: TFDQuery;
    btnToList: TButton;
    DBGrid1: TDBGrid;
    btnLoadFromAPI: TButton;
    FDMemTable1: TFDMemTable;
    FDMemTable1id: TIntegerField;
    FDMemTable1title: TStringField;
    FDMemTable1completed: TBooleanField;
    DataSource1: TDataSource;
    FDMemTable1userId: TIntegerField;
    procedure btnSaveDataSetClick(Sender: TObject);
    procedure btnToListClick(Sender: TObject);
    procedure btnLoadFromAPIClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCustomer = class
  private
    [MVCTableField('address_line1')]
    FAddressLine1: string;
    [MVCTableField('address_line2')]
    FAddressLine2: string;
    FCity: string;
    [MVCTableField('contact_first')]
    FContactFirst: string;
    [MVCTableField('contact_last')]
    FContactLast: string;
    FCountry: string;
    [MVCTableField('cust_no')]
    FCustNo: Integer;
    FCustomer: string;
    [MVCTableField('on_hold')]
    FOnHold: string;
    [MVCTableField('phone_no')]
    FPhoneNo: string;
    [MVCTableField('postal_code')]
    FPostalCode: string;
    [MVCTableField('state_province')]
    FStateProvince: string;
  public
    property AddressLine1: string read FAddressLine1 write FAddressLine1;
    property AddressLine2: string read FAddressLine2 write FAddressLine2;
    property City: string read FCity write FCity;
    property ContactFirst: string read FContactFirst write FContactFirst;
    property ContactLast: string read FContactLast write FContactLast;
    property Country: string read FCountry write FCountry;
    property CustNo: Integer read FCustNo write FCustNo;
    property Customer: string read FCustomer write FCustomer;
    property OnHold: string read FOnHold write FOnHold;
    property PhoneNo: string read FPhoneNo write FPhoneNo;
    property PostalCode: string read FPostalCode write FPostalCode;
    property StateProvince: string read FStateProvince write FStateProvince;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Generics.Collections, MVCFramework.AsyncTask,
  MVCFramework.RESTClient, JsonDataObjects, MVCFramework.Serializer.Commons;

{$R *.dfm}

procedure TMainForm.btnLoadFromAPIClick(Sender: TObject);
begin
  var lSavedCaption := btnLoadFromAPI.Caption;
  btnLoadFromAPI.Caption := 'Loading...';
  btnLoadFromAPI.Enabled := False;
  FDMemTable1.EmptyDataSet;
  MVCAsyncObject.Run<TJSONArray>(
    function : TJSONArray
    begin
      Sleep(500);
      Result := TMVCRESTClient.New
        .BaseURL('https://jsonplaceholder.typicode.com')
        .Get('/todos')
        .ToJSONArray;
    end,
    procedure (const JSONArr: TJsonArray)
    begin
      FDMemTable1.LoadFromJSONArray(JSONArr, TMVCNameCase.ncAsIs);
      FDMemTable1.First;
    end,
    nil,
    procedure
    begin
      btnLoadFromAPI.Caption := lSavedCaption;
      btnLoadFromAPI.Enabled := True;
    end);
end;

procedure TMainForm.btnSaveDataSetClick(Sender: TObject);
begin
  CustomerTable.Close();
  CustomerTable.Open();
  TFile.WriteAllText('customers.json', CustomerTable.AsJSONArray());
  CustomerTable.Close();
end;

procedure TMainForm.btnToListClick(Sender: TObject);
begin
  CustomerTable.Close();
  CustomerTable.Open();
  var lCustomers: TObjectList<TCustomer> := CustomerTable.AsObjectList<TCustomer>();
  try
    ShowMessage(lCustomers.First.Customer);
  finally
    lCustomers.Free;
  end;
  CustomerTable.Close();
end;

end.
