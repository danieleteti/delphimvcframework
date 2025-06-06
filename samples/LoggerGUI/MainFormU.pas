unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MVCFramework.Logger;

type
  TMainForm = class(TForm)
    btnLoggerTest: TButton;
    procedure btnLoggerTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnLoggerTestClick(Sender: TObject);
begin
  Log.Info('This is an info log', 'log1');
  Log.Warn('This is a warn log', 'log1');
  Log.Debug('This is a debug log', 'log2');
  Log.Error('This is an error log', 'log2');
  Log.Fatal('This is a fatal log', 'log3');
end;

end.
