unit EngineChoiceFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TRDBMSEngine = (PostgreSQL, Firebird, Interbase, MSSQLServer, MySQL, MariaDB, SQLite);

  TEngineChoiceForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
  private
    fSelectedRDBMS: TRDBMSEngine;
    function SelectedRDBMS: TRDBMSEngine;
  public
    class function Execute: TRDBMSEngine;
  end;

implementation

{$R *.dfm}

procedure TEngineChoiceForm.Button1Click(Sender: TObject);
begin
  fSelectedRDBMS := TRDBMSEngine(TComponent(Sender).Tag);
  ModalResult := mrOk;
end;

class function TEngineChoiceForm.Execute: TRDBMSEngine;
var
  lFrm: TEngineChoiceForm;
begin
  Result := TRDBMSEngine.PostgreSQL;
  lFrm := TEngineChoiceForm.Create(nil);
  try
    if lFrm.ShowModal = mrOk then
    begin
      Result := lFrm.SelectedRDBMS;
    end
    else
    begin
      Application.Terminate;
    end;
  finally
    lFrm.Free;
  end;
end;

function TEngineChoiceForm.SelectedRDBMS: TRDBMSEngine;
begin
  Result := fSelectedRDBMS;
end;

end.
