unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  MVCFramework.Logger;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
    [MVCPath('/profilersample1')]
    [MVCHTTPMethod([httpGET])]
    procedure ProfilerSample1;
  protected
    fCalls: Integer;
    procedure ProcA;
    procedure ProcB;
    procedure DoSomething;
    procedure DoSomethingElse;
    procedure NotProfiled;
  end;

implementation

uses
  System.SysUtils, System.StrUtils;

procedure TMyController.ProfilerSample1;
begin
  NotProfiled(); //this line is not profiled

  //the following begin..end block will be profiled
  //timing will be saved in a "profiler" log
  begin var lProf := Profiler.Start(Context.ActionQualifiedName);
    DoSomething();
    DoSomethingElse();
    Render('Just executed ' + Context.ActionQualifiedName);
  end; // profiler writes to the log

  NotProfiled(); //this line is not profiled
end;

procedure TMyController.DoSomething;
begin
  begin var lProf := Profiler.Start('DoSomething');
    Sleep(100);
  end;
end;

procedure TMyController.DoSomethingElse;
begin
  begin var lProf := Profiler.Start('DoSomethingElse');
    Sleep(100);
    DoSomething();
  end;
end;

procedure TMyController.NotProfiled;
begin
  Sleep(100);
end;

procedure TMyController.Index;
begin
  fCalls := 0;
  begin var lProf := Profiler.Start(Context.ActionQualifiedName, []);
    ProcA;
    Render('Hello DelphiMVCFramework (profiled) World');
  end;
end;

procedure TMyController.ProcA;
begin
  begin var lProf := Profiler.Start('TMyController.ProcA');
    Inc(fCalls);
    ProcB;
  end;
end;

procedure TMyController.ProcB;
begin
  begin var lProf := Profiler.Start('TMyController.ProcB');
    Inc(fCalls);
    if fCalls < 20 then
    begin
      ProcA;
    end;
  end;
end;



end.
