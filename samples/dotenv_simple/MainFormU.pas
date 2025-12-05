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
  try
    //optional, checks if required keys are available
    //dotEnv.RequireKeys(['mode','dbuser','dbpassword','dbhostname']);

    Memo1.Lines.Add('=== BASIC CONFIGURATION VALUES ===');
    Memo1.Lines.Add('mode       : ' + dotEnv.Env('mode'));
    Memo1.Lines.Add('dbuser     : ' + dotEnv.Env('dbuser'));
    Memo1.Lines.Add('dbpassword : ' + dotEnv.Env('dbpassword'));
    Memo1.Lines.Add('dbhostname : ' + dotEnv.Env('dbhostname'));
    Memo1.Lines.Add('dbport     : ' + dotEnv.Env('dbport'));
    Memo1.Lines.Add('api_version: ' + dotEnv.Env('api_version'));
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== SIMPLE MATHEMATICAL EXPRESSIONS ===');
    Memo1.Lines.Add('simple_sum       : ' + dotEnv.Env('simple_sum') + ' (from: $[2 + 3])');
    Memo1.Lines.Add('simple_multiply  : ' + dotEnv.Env('simple_multiply') + ' (from: $[10 * 5])');
    Memo1.Lines.Add('division_result  : ' + dotEnv.Env('division_result') + ' (from: $[100 / 4])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== EXPRESSIONS USING VARIABLES ===');
    Memo1.Lines.Add('base_memory      : ' + dotEnv.Env('base_memory'));
    Memo1.Lines.Add('max_connections  : ' + dotEnv.Env('max_connections') + ' (from: $[100 + 50])');
    Memo1.Lines.Add('timeout_seconds  : ' + dotEnv.Env('timeout_seconds') + ' (from: $[30 * 60])');
    Memo1.Lines.Add('buffer_size      : ' + dotEnv.Env('buffer_size') + ' (from: $[base_memory * base_memory])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== STRING CONVERSION FUNCTIONS ===');
    Memo1.Lines.Add('db_host_str : ' + dotEnv.Env('db_host_str') + ' (from: $[ToString(dbhostname)])');
    Memo1.Lines.Add('port_str    : ' + dotEnv.Env('port_str') + ' (from: $[ToString(dbport)])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== MATHEMATICAL FUNCTIONS ===');
    Memo1.Lines.Add('sqrt_result : ' + dotEnv.Env('sqrt_result') + ' (from: $[sqrt(16)])');
    Memo1.Lines.Add('log_result  : ' + dotEnv.Env('log_result') + ' (from: $[log(100)])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== BOOLEAN EXPRESSIONS ===');
    Memo1.Lines.Add('high_memory     : ' + dotEnv.Env('high_memory') + ' (from: $[base_memory > 512])');
    Memo1.Lines.Add('low_connections : ' + dotEnv.Env('low_connections') + ' (from: $[max_connections < 200])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== COMPLEX CALCULATIONS ===');
    Memo1.Lines.Add('memory_per_connection: ' + dotEnv.Env('memory_per_connection') + ' (from: $[buffer_size / max_connections])');
    Memo1.Lines.Add('total_timeout       : ' + dotEnv.Env('total_timeout') + ' (from: $[timeout_seconds + 300])');
    Memo1.Lines.Add('');

    Memo1.Lines.Add('=== OPTIONALS WITH DEFAULT ===');
    Memo1.Lines.Add('user_preferences_path : ' + dotEnv.Env('user_preferences_path', '.\myfolder'));

  except
    on E: Exception do
    begin
      Memo1.Lines.Add('ERROR: ' + E.Message);
      Memo1.Lines.Add('Exception Class: ' + E.ClassName);
    end;
  end;
end;

end.

