unit RESTAppenderFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  LoggerPro,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  LoggerProConfig;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Log.Debug('This is a debug message with TAG1', 'TAG1');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Log.Info('This is a info message with TAG1', 'TAG1');
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  Log.Warn('This is a warning message with TAG1', 'TAG1');
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  Log.Error('This is a error message with TAG1', 'TAG1');
end;

procedure TMainForm.Button5Click(Sender: TObject);
var
  lThreadProc: TProc;
begin
  lThreadProc := procedure
    var
      I: Integer;
      lThreadID: string;
    begin
      lThreadID := IntToStr(TThread.Current.ThreadID);
      for I := 1 to 100 do
      begin
        Log.Debug('log message %s ThreadID: %s', [TimeToStr(now), lThreadID], 'MULTITHREADING');
        Log.Info('log message %s ThreadID: %s', [TimeToStr(now), lThreadID], 'MULTITHREADING');
        Log.Warn('log message %s ThreadID: %s', [TimeToStr(now), lThreadID], 'MULTITHREADING');
        Log.Error('log message %s ThreadID: %s', [TimeToStr(now), lThreadID], 'MULTITHREADING');
      end;
    end;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
end;

end.
