unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MVCFramework.ObjectPool;

type
  TMainForm = class(TForm)
    btnUnlimitedPool: TButton;
    lbLog: TListBox;
    btnPlain: TButton;
    Label1: TLabel;
    btnPoolSize: TButton;
    procedure btnUnlimitedPoolClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPlainClick(Sender: TObject);
    procedure btnPoolSizeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPoolObject = class
  strict private
    fValue: integer;
  public
    constructor Create; overload;
    constructor Create(const Value: integer); overload;
    property Value: integer read FValue;
  end;

var
  MainForm: TMainForm;

implementation

uses
  MVCFramework.Logger, LoggerPro, LoggerPro.VCLListBoxAppender, System.Threading, LoggerPro.Renderers;

{$R *.dfm}

var
  lPool: IObjectPool<TPoolObject> = nil;

procedure DoUnlimitedSizePoolTest;
begin
  var lProc: TProc :=  procedure
                       var
                         lObjs: array [1 .. 50] of TPoolObject;
                       begin
                         for var lRpt := 1 to 10 do
                         begin
                           { get from and release to ... pool }
                           for var i := Low(lObjs) to High(lObjs) do
                             lObjs[i] := lPool.GetFromPool(False);
                           for var i := Low(lObjs) to High(lObjs) do
                             lPool.ReleaseToPool(lObjs[i]);
                         end;
                       end;
  var lTasks: TArray<ITask> := [
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc)
  ];

  TTask.WaitForAll(lTasks);
end;

procedure DoNoPoolTest;
begin
  var lProc: TProc :=  procedure
                       var
                         lObjs: array [1 .. 50] of TPoolObject;
                       begin
                         for var lRpt := 1 to 10 do
                         begin
                           for var i := Low(lObjs) to High(lObjs) do
                             lObjs[i] := TPoolObject.Create;
                           for var i := Low(lObjs) to High(lObjs) do
                             lObjs[i].Free;
                         end;
                       end;
  var lTasks: TArray<ITask> := [
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc)
  ];

  TTask.WaitForAll(lTasks);
end;


{ TPoolObject }

constructor TPoolObject.Create;
begin
  Create(1);
end;

constructor TPoolObject.Create(const Value: integer);
begin
  inherited Create;
  FValue := Value;
  Sleep(1); //some heady initialization
end;

procedure TMainForm.btnPlainClick(Sender: TObject);
begin
  Profiler.Trace('DoNoPoolTest'.PadRight(25), procedure begin DoNoPoolTest; end, 0);
end;

procedure TMainForm.btnPoolSizeClick(Sender: TObject);
begin
  Profiler.ProfileLogger.Log(TLogType.Info, 'Current Pool Size: %4d', [lPool.Size], 'misc');
end;

procedure TMainForm.btnUnlimitedPoolClick(Sender: TObject);
begin
  Profiler.Trace('DoUnlimitedSizePoolTest'.PadRight(25), procedure begin DoUnlimitedSizePoolTest; end, 0);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Profiler.ProfileLogger := BuildLogWriter([TVCLListBoxAppender.Create(lbLog)]);
end;

initialization

lPool := TPoolFactory.CreateUnlimitedPool<TPoolObject>(
    20 {ShrinkTriggerSize},
    10 {ShrinkTargetSize},
    nil {Optional Factory Function});
end.
