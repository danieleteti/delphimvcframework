unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Intf, FireDAC.Comp.UI;

type
  TMainForm = class(TForm)
    btnDataPump: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnDataPumpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  FDConnectionConfigU, MVCFramework.ActiveRecord, CustomerEntityU,
  MVCFramework.SQLGenerators.PostgreSQL, MVCFramework.SQLGenerators.Sqlite;

procedure TMainForm.btnDataPumpClick(Sender: TObject);
begin
  // let's prepare connections giving them a name
  ActiveRecordConnectionsRegistry.AddConnection('source', 'sqlitecon');
  try
    ActiveRecordConnectionsRegistry.AddConnection('destination', 'pgcon');
    try

      // from now on, in this thread, current connection is "source"
      ActiveRecordConnectionsRegistry.SetCurrent('source');
      var lCustomers := TMVCActiveRecord.All<TCustomer>;
      try

        // from now on, in this thread, current connection is "destination"
        ActiveRecordConnectionsRegistry.SetCurrent('destination');
        begin var lTx := TMVCActiveRecord.UseTransactionContext; //use TransactionContext
          for var lCustomer in lCustomers do
          begin
            lCustomer.InvalidateConnection; //set connection to the current one
            lCustomer.Insert;
          end;
        end;

      finally
        lCustomers.Free;
      end;
    finally
      ActiveRecordConnectionsRegistry.RemoveConnection('destination');
    end;
  finally
    ActiveRecordConnectionsRegistry.RemoveConnection('source');
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateSqlitePrivateConnDef(False, 'sqlitecon');
  CreatePostgresqlPrivateConnDef(False, 'pgcon');
end;

end.
