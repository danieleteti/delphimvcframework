unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    btnSimple: TButton;
    btnNestedCalls: TButton;
    btnNestedCallsInLoop: TButton;
    chkLogsThreshold: TCheckBox;
    procedure btnSimpleClick(Sender: TObject);
    procedure btnNestedCallsClick(Sender: TObject);
    procedure btnNestedCallsInLoopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkLogsThresholdClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ManyNestedCalls;
    procedure NestedCalls;
  protected
    fCalls: Integer;
    procedure ProcA;
    procedure ProcB;
    procedure DoSomething;
    procedure DoSomethingElse;
    procedure NotProfiled;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MVCFramework.Logger;

procedure TMainForm.NestedCalls;
begin
  NotProfiled(); //this line is not profiled

  //the following begin..end block will be profiled
  //timing will be saved in a "profiler" log
  begin var lProf := Profiler.Start('NestedCalls');
    DoSomething();
    DoSomethingElse();
  end; // profiler writes to the log

  NotProfiled(); //this line is not profiled
end;

procedure TMainForm.btnNestedCallsClick(Sender: TObject);
begin
  NestedCalls;
end;

procedure TMainForm.btnNestedCallsInLoopClick(Sender: TObject);
begin
  ManyNestedCalls;
end;

procedure TMainForm.btnSimpleClick(Sender: TObject);
begin
  DoSomething;
end;

procedure TMainForm.chkLogsThresholdClick(Sender: TObject);
begin
  Profiler.LogsOnlyIfOverThreshold := chkLogsThreshold.Checked;
end;

procedure TMainForm.DoSomething;
begin
  begin var lProf := Profiler.Start('DoSomething');
    Sleep(500);
  end;
end;

procedure TMainForm.DoSomethingElse;
begin
  begin var lProf := Profiler.Start('DoSomethingElse');
    Sleep(1000);
    DoSomething();
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  chkLogsThreshold.Checked := Profiler.LogsOnlyIfOverThreshold;
end;

procedure TMainForm.NotProfiled;
begin
  Sleep(100);
end;

procedure TMainForm.ManyNestedCalls;
begin
  fCalls := 0;
  begin var lProf := Profiler.Start('ManyNestedCalls');
    ProcA;
  end;
end;

procedure TMainForm.ProcA;
begin
  begin var lProf := Profiler.Start('TMainForm.ProcA');
    Inc(fCalls);
    ProcB;
  end;
end;

procedure TMainForm.ProcB;
begin
  begin var lProf := Profiler.Start('TMainForm.ProcB');
    Inc(fCalls);
    if fCalls < 20 then
    begin
      ProcA;
    end;
  end;
end;


initialization

Profiler.ProfileLogger := Log;

end.
