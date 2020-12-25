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
  MVCFramework.DataSet.Utils,
  MVCFramework.Commons;

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
