unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  MVCFramework.ActiveRecord, FireDAC.Phys.PG,
  MVCFramework.SQLGenerators.PostgreSQL,
  FireDAC.VCLUI.Wait ;

type
  TForm5 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses EntitiesU;

procedure TForm5.FormCreate(Sender: TObject);
begin
  ActiveRecordConnectionsRegistry.AddDefaultConnection('activerecorddb');

  var lList := TMVCActiveRecord.All<TCustomers>;
  try
    lList[0].City := 'rome';
    lList[0].Code.Clear;
    lList[0].Store;
  finally
    lList.Free;
  end;

end;

end.
