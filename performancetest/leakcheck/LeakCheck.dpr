program LeakCheck;

{
  Empirical proof for issue #894.

  The original streaming serializer kept TStreamWriter / TJsonTextWriter
  in threadvars across requests. When a thread terminated those objects
  were leaked because Delphi has no destructor hook for threadvar slots.

  This test exercises TryWriteObject / TryWriteList / TryWriteDataSet
  from many short-lived TThreads, then exits with FastMM's
  ReportMemoryLeaksOnShutdown enabled. With the threadvar in place this
  used to print `2 * thread_count` unexpected leaks (one TStreamWriter
  plus one TJsonTextWriter per worker). After the per-call refactor it
  must print clean.

  Pass criterion: process exits with zero leaks reported by the RTL.
  Any leak printed by FastMM means the fix regressed.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Data.DB,
  Datasnap.DBClient,
  MVCFramework.Serializer.Streaming;

type
  TPerson = class
  private
    FId: Integer;
    FName: string;
    FAge: Integer;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  TPersonList = class(TObjectList<TPerson>);

  TWorker = class(TThread)
  private
    FIterations: Integer;
    FErrorMsg: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AIterations: Integer);
    property ErrorMsg: string read FErrorMsg;
  end;

constructor TWorker.Create(AIterations: Integer);
begin
  FIterations := AIterations;
  FErrorMsg := '';
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure ExerciseObject(AIter: Integer);
var
  LObj: TPerson;
  LMS: TMemoryStream;
begin
  LObj := TPerson.Create;
  try
    LObj.Id := AIter;
    LObj.Name := 'Name_' + IntToStr(AIter);
    LObj.Age := 30 + (AIter mod 50);
    LMS := TMemoryStream.Create;
    try
      TMVCStreamingJsonSerializer.TryWriteObject(LObj, LMS);
    finally
      LMS.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure ExerciseList;
var
  LList: TPersonList;
  LMS: TMemoryStream;
  LP: TPerson;
  I: Integer;
begin
  LList := TPersonList.Create(True);
  try
    for I := 0 to 9 do
    begin
      LP := TPerson.Create;
      LP.Id := I;
      LP.Name := 'P' + IntToStr(I);
      LP.Age := 20 + I;
      LList.Add(LP);
    end;
    LMS := TMemoryStream.Create;
    try
      TMVCStreamingJsonSerializer.TryWriteList(LList, LMS);
    finally
      LMS.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure ExerciseDataSet;
var
  LDS: TClientDataSet;
  LMS: TMemoryStream;
  I: Integer;
begin
  LDS := TClientDataSet.Create(nil);
  try
    LDS.FieldDefs.Add('id', ftInteger);
    LDS.FieldDefs.Add('name', ftString, 64);
    LDS.CreateDataSet;
    for I := 0 to 9 do
    begin
      LDS.Append;
      LDS.FieldByName('id').AsInteger := I;
      LDS.FieldByName('name').AsString := 'Row_' + IntToStr(I);
      LDS.Post;
    end;
    LMS := TMemoryStream.Create;
    try
      TMVCStreamingJsonSerializer.TryWriteDataSet(LDS, LMS);
    finally
      LMS.Free;
    end;
  finally
    LDS.Free;
  end;
end;

procedure TWorker.Execute;
var
  I: Integer;
begin
  try
    for I := 1 to FIterations do
    begin
      ExerciseObject(I);
      if (I mod 5) = 0 then ExerciseList;
      if (I mod 10) = 0 then ExerciseDataSet;
    end;
  except
    on E: Exception do
      FErrorMsg := E.ClassName + ': ' + E.Message;
  end;
end;

function RunMultiThreadStress(AThreadCount, AIterationsPerThread: Integer): Integer;
var
  LWorkers: TArray<TWorker>;
  I: Integer;
  LFailed: Integer;
begin
  Writeln(Format('Spawning %d threads x %d iterations each...',
    [AThreadCount, AIterationsPerThread]));
  SetLength(LWorkers, AThreadCount);
  for I := 0 to AThreadCount - 1 do
    LWorkers[I] := TWorker.Create(AIterationsPerThread);

  LFailed := 0;
  for I := 0 to AThreadCount - 1 do
  begin
    LWorkers[I].WaitFor;
    if LWorkers[I].ErrorMsg <> '' then
    begin
      Writeln('Worker ', I, ' failed: ', LWorkers[I].ErrorMsg);
      Inc(LFailed);
    end;
    LWorkers[I].Free;
  end;
  Writeln(Format('All threads terminated. Failures: %d', [LFailed]));
  Result := LFailed;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if (ParamCount = 1) and (ParamStr(1) = '--canary') then
    begin
      { Negative control: leak one object on purpose. Proves the RTL leak
        detector is wired and reachable in this build configuration.
        If the canary run prints NO leak, the detector is broken and the
        clean run above means nothing. }
      Writeln('CANARY MODE: intentionally leaking one TPerson.');
      Writeln('Expect the RTL to report exactly one TPerson leak below.');
      TPerson.Create;  // never freed
      ExitCode := 0;
      Exit;
    end;

    if RunMultiThreadStress(50, 200) <> 0 then
    begin
      ExitCode := 1;
      Exit;
    end;
    Writeln;
    Writeln('=== Worker threads have all terminated. ===');
    Writeln('=== Any leaks printed below this line by the RTL = FAIL. ===');
    Writeln('=== Silent exit = PASS. ===');
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('FATAL: ', E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
