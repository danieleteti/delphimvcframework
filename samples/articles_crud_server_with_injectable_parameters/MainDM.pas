unit MainDM;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.FB,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.FBDef,
  FireDAC.VCLUI.Wait;

type
  TdmMain = class(TDataModule)
    Connection: TFDConnection;
    dsArticles: TFDQuery;
    updArticles: TFDUpdateSQL;
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    function SearchProducts(const SearchText: string): TDataSet;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.IOUtils,
  MVCFramework.DataSet.Utils;

procedure TdmMain.ConnectionBeforeConnect(Sender: TObject);
var
  I: Integer;
  lPath: string;
begin
  {
    This code is just a demo. It looks for a db file doing up to 6 attempts.
    I need this becouse this unit is used by many samples and these samples are
    compiled at different level in the samples folders tree.
    In a real word system you should (!!) know where your database is :-)
  }
  lPath := 'data\ORDERSMANAGER_FB30.FDB';
  for I := 1 to 6 do
  begin
    if TFile.Exists(lPath) then
    begin
      Connection.Params.Values['Database'] := TPath.GetFullPath(lPath);
      // 'C:\DEV\dmvcframework\samples\data\ORDERSMANAGER_FB30.FDB';
      Break;
    end
    else
    begin
      lPath := '..\' + lPath;
    end;
  end;
  if not TFile.Exists(lPath) then
  begin
    raise Exception.Create('I tried hard, but I cannot find the database');
  end;
end;

function TdmMain.SearchProducts(const SearchText: string): TDataSet;
begin
  Result := TFDMemTable.Create(nil);
  if SearchText.IsEmpty then
    dsArticles.Open('SELECT * FROM ARTICOLI')
  else
    dsArticles.Open('SELECT * FROM ARTICOLI WHERE DESCRIZIONE CONTAINING ?', [SearchText]);
  TFDTable(Result).CopyDataSet(dsArticles, [coStructure, coRestart, coAppend]);
end;

end.
