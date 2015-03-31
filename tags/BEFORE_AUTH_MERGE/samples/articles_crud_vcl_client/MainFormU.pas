unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.StdCtrls, MVCFramework.RESTClient,
  Vcl.DBCtrls;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    FDMemTable1: TFDMemTable;
    FDMemTable1id: TIntegerField;
    FDMemTable1code: TStringField;
    FDMemTable1description: TStringField;
    FDMemTable1price: TCurrencyField;
    DataSource1: TDataSource;
    btnGetListAsynch: TButton;
    btnGetListSynch: TButton;
    DBNavigator1: TDBNavigator;
    procedure Button1Click(Sender: TObject);
    procedure btnGetListAsynchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FDMemTable1BeforePost(DataSet: TDataSet);
    procedure FDMemTable1BeforeDelete(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    CltAsynch: TRESTClient;
    FLoading: Boolean;
    Clt: TRESTClient;
    { Private declarations }
    procedure ShowError(const AResponse: IRESTResponse);
  public
    procedure RefreshAsynch;
  end;

var
  Form4: TForm4;

implementation

uses
  ObjectsMappers;

{$R *.dfm}


procedure TForm4.btnGetListAsynchClick(Sender: TObject);
begin
  // this an asychronous request... just like you could do in jQuery
  CltAsynch.Asynch(
    procedure(Res: IRESTResponse)
    begin
      FDMemTable1.Close;
      FDMemTable1.Open;
      FLoading := true;
      FDMemTable1.AppendFromJSONArrayString(Res.BodyAsString);
      FLoading := false;
    end,
    nil, nil, true)
    .doGET('/articles', []);
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  RefreshAsynch;
end;

procedure TForm4.FDMemTable1BeforeDelete(DataSet: TDataSet);
var
  Res: IRESTResponse;
begin
  if FDMemTable1.State = dsBrowse then
    Res := Clt.DSDelete('/articles', FDMemTable1id.AsString);
  if not(Res.ResponseCode in [200, 201]) then
  begin
    ShowError(Res);
    Abort;
  end
  else
    Refresh;
end;

procedure TForm4.FDMemTable1BeforePost(DataSet: TDataSet);
var
  Res: IRESTResponse;
begin
  if not FLoading then
  begin
    if FDMemTable1.State = dsInsert then
      Res := Clt.dsInsert('/articles', FDMemTable1)
    else
      Res := Clt.dsUpdate('/articles', FDMemTable1, FDMemTable1id.AsString);
    if not(Res.ResponseCode in [200, 201]) then
    begin
      ShowError(Res);
      Abort;
    end
    else
      RefreshAsynch;
  end;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CltAsynch.Free;
  Clt.Free;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Clt := TRESTClient.Create('localhost', 8080);

  // just for demo, here's the asych version. It is the same :-)
  // check the GETLISTASYNCH button
  CltAsynch := TRESTClient.Create('localhost', 8080);
end;

procedure TForm4.RefreshAsynch;
var
  Res: IRESTResponse;
begin
  // this a simple sychronous request...
  Res := Clt.doGET('/articles', []);
  FDMemTable1.Close;
  FDMemTable1.Open;
  FLoading := true;
  FDMemTable1.AppendFromJSONArrayString(Res.BodyAsString);
  FLoading := false;
end;

procedure TForm4.ShowError(const AResponse: IRESTResponse);
begin
  ShowMessage(
    AResponse.ResponseCode.ToString + ': ' + AResponse.ResponseText + sLineBreak +
    AResponse.BodyAsJsonObject.Get('message').JsonValue.Value);
end;

end.
