program repro_Depth;
{ Minimal reproduction: TJsonReader is recursive-descent and has no nesting limit,
  so the nesting depth of the input decides how much stack the parser uses.

  Build it against an UNPATCHED Source/JsonDataObjects.pas:

    dcc32 -B -U<path-to>\JsonDataObjects\Source repro_Depth.dpr
    dcc64 -B -U<path-to>\JsonDataObjects\Source repro_Depth.dpr

  Then walk the depth up:

    repro_Depth 1000     ->  parses
    repro_Depth 8000     ->  EStackOverflow, still catchable (Win64)
    repro_Depth 8500     ->  no output after "IsValidJSON...", exit code 139

  The payload at that last step is ~34 KB of perfectly valid JSON. Note that
  IsValidJSON() dies too: its except-clause cannot catch what does not raise. }

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  JsonDataObjects;

function NestedArrays(const ADepth: Integer): string;
var
  SB: TStringBuilder;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    for I := 1 to ADepth do
      SB.Append('[');
    SB.Append('1');
    for I := 1 to ADepth do
      SB.Append(']');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure Say(const AText: string);
begin
  Writeln(AText);
  Flush(Output); // the process may not survive the next line
end;

var
  Depth: Integer;
  JSON: string;
  Obj: TJsonBaseObject;
begin
  Depth := StrToIntDef(ParamStr(1), 8500);
  JSON := NestedArrays(Depth);
  Say(Format('%d-bit, depth=%d, payload=%d bytes',
    [SizeOf(Pointer) * 8, Depth, Length(JSON) * SizeOf(Char)]));

  Say('IsValidJSON...');
  try
    Say('  IsValidJSON = ' + BoolToStr(IsValidJSON(JSON), True));
  except
    on E: Exception do
      Say('  ' + E.ClassName + ': ' + E.Message);
  end;

  Say('Parse...');
  try
    Obj := TJsonBaseObject.Parse(JSON);
    try
      Say('  parsed');
    finally
      Obj.Free;
    end;
  except
    on E: Exception do
      Say('  ' + E.ClassName + ': ' + E.Message);
  end;

  Say('done');
end.
