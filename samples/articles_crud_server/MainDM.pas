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
  FireDAC.VCLUI.Wait, Services;

type
  TdmMain = class(TDataModule)
    Connection: TFDConnection;
    dsArticles: TFDQuery;
    updArticles: TFDUpdateSQL;
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create; reintroduce;
    function SearchProducts(const SearchText: string): TDataSet;

  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.DataSet.Utils;

procedure TdmMain.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Values['Database'] := dotEnv.Env('database.path');
end;

constructor TdmMain.Create;
begin
  inherited Create(nil);
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
