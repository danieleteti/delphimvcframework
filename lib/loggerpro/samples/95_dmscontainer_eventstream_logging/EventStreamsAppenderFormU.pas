unit EventStreamsAppenderFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, LoggerPro, Vcl.ExtCtrls;

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
    procedure FormCreate(Sender: TObject);
  private
    fContext: string;
    { Private declarations }
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


uses LoggerProConfig;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Log.Debug('This is a debug message with TAG1 (%s)', [fContext], 'TAG1');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Log.Info('This is a info message with TAG1 (%s)', [fContext], 'TAG1');
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  Log.Warn('This is a warning message with TAG1 (%s)', [fContext], 'TAG1');
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  Log.Error('This is a error message with TAG1 (%s)', [fContext], 'TAG1');
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
        Log.Debug('log message ' + TimeToStr(now),// + ' ThreadID: ' + lThreadID,
          'MULTITHREADING');
        Log.Info('log message ' + TimeToStr(now),// + ' ThreadID: ' + lThreadID,
          'MULTITHREADING');
        Log.Warn('log message ' + TimeToStr(now),// + ' ThreadID: ' + lThreadID,
          'MULTITHREADING');
        Log.Error('log message ' + TimeToStr(now),// + ' ThreadID: ' + lThreadID,
          'MULTITHREADING');
      end;
    end;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
  TThread.CreateAnonymousThread(lThreadProc).Start;
end;

function GetUserFromWindows: string;
var
  iLen: Cardinal;
begin
  iLen := 256;
  Result := StringOfChar(#0, iLen);
  GetUserName(PChar(Result), iLen);
  SetLength(Result, iLen-1);
end;

function GetComputerNameFromWindows: string;
var
  iLen: Cardinal;
begin
  iLen := MAX_COMPUTERNAME_LENGTH + 1;
  Result := StringOfChar(#0, iLen);
  GetComputerName(PChar(Result), iLen);
  SetLength(Result, iLen);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fContext := GetUserFromWindows + '@' + GetComputerNameFromWindows;
end;

end.
