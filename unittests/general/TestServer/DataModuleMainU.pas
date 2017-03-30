unit DataModuleMainU;

interface

uses
  System.SysUtils,
  System.Classes,
  {
    uADStanIntf,
    uADStanOption,
    uADStanParam,
    uADStanError,
    uADDatSManager,
    uADPhysIntf,
    uADDAptIntf,
    uADCompDataSet,
    uADCompClient}
  Data.DB;

type
  TDataModuleMain = class(TDataModule)

  private
    { Private declarations }
  public
    function GetMemTable: TDataSet;
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

uses
  ioutils;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}


function TDataModuleMain.GetMemTable: TDataSet;
begin
  // FMemTable := TFDMemTable.Create(Self);
  // MemTable.FileName := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'testdata.xml');
  // if tfile.Exists(MemTable.FileName) then
  // MemTable.LoadFromFile
  // else
  // MemTable.Open;
end;

end.
