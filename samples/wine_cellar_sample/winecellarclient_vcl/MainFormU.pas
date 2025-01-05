unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Mask, Vcl.DBCtrls, Vcl.ExtCtrls,
  MVCFramework.RESTClient.Intf, MVCFramework.RESTClient;

type
  TForm5 = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    DBGrid1: TDBGrid;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    FDMemTable1id: TIntegerField;
    FDMemTable1name: TStringField;
    FDMemTable1year: TStringField;
    FDMemTable1grapes: TStringField;
    FDMemTable1country: TStringField;
    FDMemTable1region: TStringField;
    FDMemTable1description: TMemoField;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    procedure Button1Click(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FDMemTable1BeforePost(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FDMemTable1BeforeDelete(DataSet: TDataSet);

  private
    RESTClient: IMVCRESTClient;
    Loading: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  MVCFramework.DataSet.Utils;

{$R *.dfm}


procedure TForm5.Button1Click(Sender: TObject);
begin
  RESTClient.Async(
    procedure (Resp: IMVCRESTResponse)
    begin
      Memo1.Lines.Text := Resp.Content;
      FDMemTable1.Close;
      FDMemTable1.Open;
      Loading := True;
      FDMemTable1.AppendFromJSONArrayString(Resp.Content);
      FDMemTable1.First;
      Loading := False;
    end, nil, True).Get('/api/wines');
end;

procedure TForm5.DBGrid1DblClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm5.FDMemTable1BeforeDelete(DataSet: TDataSet);
var
  Resp: IMVCRESTResponse;
begin
  Resp := RESTClient.DataSetDelete('/api/wines', FDMemTable1id.AsString);
  if not Resp.StatusCode in [200] then
    raise Exception.Create(Resp.Content);
end;

procedure TForm5.FDMemTable1BeforePost(DataSet: TDataSet);
var
  Resp: IMVCRESTResponse;
begin
  if Loading then
    Exit;
  case FDMemTable1.State of
    dsEdit:
      Resp := RESTClient.DataSetUpdate('/api/wines', FDMemTable1id.AsString, FDMemTable1);
    dsInsert:
      Resp := RESTClient.DataSetInsert('/api/wines', FDMemTable1);
  end;
  if not Resp.StatusCode in [200, 201] then
    raise Exception.Create(Resp.Content);
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RESTClient := nil;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  RESTClient := TMVCRESTClient.New.BaseURL('localhost', 3000);
end;

end.
