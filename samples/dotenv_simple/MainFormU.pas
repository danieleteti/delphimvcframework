unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MVCFramework.Commons, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //optional, checks if required keys are available
  //dotEnv.RequireKeys(['mode','dbuser','dbpassword','dbhostname']);

  Memo1.Lines.Add('mode       : ' + dotEnv.Env('mode'));
  Memo1.Lines.Add('dbuser     : ' + dotEnv.Env('dbuser'));
  Memo1.Lines.Add('dbpassword : ' + dotEnv.Env('dbpassword'));
  Memo1.Lines.Add('dbhostname : ' + dotEnv.Env('dbhostname'));
  Memo1.Lines.Add('** OPTIONALS CONF. USE A DEFAULT **');
  Memo1.Lines.Add('user_preferences_path : ' + dotEnv.Env('user_preferences_path', '.\myfolder'));
end;

end.

