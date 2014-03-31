unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Mask, Vcl.DBCtrls, Vcl.ExtCtrls,
  MVCFramework.RESTClient;

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
    RESTClient: TRESTClient;
    Loading: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}


uses ObjectsMappers;

procedure TForm5.Button1Click(Sender: TObject);
var
  response: IRESTResponse;
begin
  response := RESTClient.doGET('/wines', []);
  Memo1.Lines.Text := response.BodyAsString;
  FDMemTable1.Close;
  FDMemTable1.Open;
  Loading := True;
  FDMemTable1.AppendFromJSONArrayString(response.BodyAsString);
  FDMemTable1.First;
  Loading := False;
end;

procedure TForm5.DBGrid1DblClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm5.FDMemTable1BeforeDelete(DataSet: TDataSet);
var
  Resp: IRESTResponse;
begin
  Resp := RESTClient.DSDelete('/wines', FDMemTable1id.AsString);
  if not Resp.ResponseCode in [200] then
    raise Exception.Create(Resp.ResponseText);
end;

procedure TForm5.FDMemTable1BeforePost(DataSet: TDataSet);
var
  Resp: IRESTResponse;
begin
  if Loading then
    Exit;
  case FDMemTable1.State of
    dsEdit:
      Resp := RESTClient.DSUpdate('/wines', FDMemTable1, FDMemTable1id.AsString);
    dsInsert:
      Resp := RESTClient.dsInsert('/wines', FDMemTable1);
  end;
  if not Resp.ResponseCode in [200, 201] then
    raise Exception.Create(Resp.ResponseText);
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RESTClient.free;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  RESTClient := TRESTClient.Create('localhost', 3000);
end;

end.
