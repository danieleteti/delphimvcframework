unit EngineChoiceFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fSelectedRDBMS: TRDBMSEngine;
    function SelectedRDBMS: TRDBMSEngine;
  public
    class function Execute(out Engine: TRDBMSEngine): Boolean;
  end;

implementation

{$R *.dfm}


procedure TEngineChoiceForm.Button1Click(Sender: TObject);
begin
  fSelectedRDBMS := TRDBMSEngine(TComponent(Sender).Tag);
  ModalResult := mrOk;
end;

class function TEngineChoiceForm.Execute(out Engine: TRDBMSEngine): Boolean;
var
  lFrm: TEngineChoiceForm;
begin
  lFrm := TEngineChoiceForm.Create(nil);
  try
    Result := lFrm.ShowModal = mrOk;
    if Result then
    begin
      Engine := lFrm.SelectedRDBMS;
    end;
  finally
    lFrm.Free;
  end;
end;

procedure TEngineChoiceForm.FormCreate(Sender: TObject);
begin
  Shape1.Brush.Color := RGB($d6,$1e,$1e);
{$IFDEF USE_SEQUENCES}
  Button1.Enabled := False;
  Button2.Enabled := False;
  Button4.Enabled := False;
  Button5.Enabled := False;
  Button6.Enabled := False;
  Button7.Enabled := False;
  Caption := 'Use SEQUENCES';
{$ELSE}
  Button3.Enabled := False;
  Caption := 'Use RETURNING';
{$ENDIF}
end;

function TEngineChoiceForm.SelectedRDBMS: TRDBMSEngine;
begin
  Result := fSelectedRDBMS;
end;

end.
