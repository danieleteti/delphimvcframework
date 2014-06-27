unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.StdCtrls, MVCFramework.RESTClient;

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
    procedure Button1Click(Sender: TObject);
    procedure btnGetListAsynchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CltAsynch: TRESTClient;
    { Private declarations }
  public
    { Public declarations }
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
      FDMemTable1.AppendFromJSONArrayString(Res.BodyAsString);
    end,
    nil, nil, true)
    .doGET('/articles', []);
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  Clt: TRESTClient;
  Res: IRESTResponse;
begin
  // this a simple sychronous request...
  Clt := TRESTClient.Create('localhost', 8080);
  try
    Res := Clt.doGET('/articles', []);
    FDMemTable1.Close;
    FDMemTable1.Open;
    FDMemTable1.AppendFromJSONArrayString(Res.BodyAsString);
  finally
    Clt.Free;
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  CltAsynch := TRESTClient.Create('localhost', 8080);
end;

end.
