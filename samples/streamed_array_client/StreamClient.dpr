program StreamClient;

// ===========================================================================
//  Console client for the streaming endpoints of samples\streamed_array_writer.
//
//  The server never builds the whole list; this client does not have to either.
//  With "ndjson" the body is handled one line at a time while it arrives, so
//  the memory of this process stays flat. With "array" the document has to be
//  complete before it can be parsed, so the memory grows with the payload.
//
//  Run each mode in its own process and compare the peak memory it prints:
//
//    StreamClient.exe ndjson http://localhost:8991/stream/items
//    StreamClient.exe array  http://localhost:8991/stream/items
//
//  Start StreamedArrayWriterSample.exe first.
// ===========================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Net.HttpClient,
  Winapi.Windows,
  Winapi.PsAPI,
  JsonDataObjects;

type
  // Write-only stream: the HTTP client pushes the body into it as it arrives.
  // Complete lines are parsed and dropped, only the unfinished tail is kept.
  TLineStream = class(TStream)
  private
    fBuffer: TBytes;
    fLength: Integer;
    fRows: Int64;
    fAgeSum: Int64;
    procedure ProcessLine(const AStart, ALength: Integer);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Finish;
    property Rows: Int64 read fRows;
    property AgeSum: Int64 read fAgeSum;
  end;

function TLineStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TLineStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

procedure TLineStream.ProcessLine(const AStart, ALength: Integer);
var
  lJSON: TJsonObject;
begin
  if ALength = 0 then
    Exit;
  lJSON := TJsonObject.Parse(TEncoding.UTF8.GetString(fBuffer, AStart, ALength)) as TJsonObject;
  try
    Inc(fRows);
    Inc(fAgeSum, lJSON.I['age']);
  finally
    lJSON.Free;
  end;
end;

function TLineStream.Write(const Buffer; Count: Longint): Longint;
var
  lStart, lEnd: Integer;
begin
  Result := Count;
  if Length(fBuffer) < fLength + Count then
    SetLength(fBuffer, (fLength + Count) * 2);
  Move(Buffer, fBuffer[fLength], Count);
  Inc(fLength, Count);

  lStart := 0;
  for lEnd := 0 to fLength - 1 do
    if fBuffer[lEnd] = 10 then
    begin
      ProcessLine(lStart, lEnd - lStart);
      lStart := lEnd + 1;
    end;
  // keep the unfinished tail only
  if lStart > 0 then
  begin
    Move(fBuffer[lStart], fBuffer[0], fLength - lStart);
    Dec(fLength, lStart);
  end;
end;

procedure TLineStream.Finish;
begin
  ProcessLine(0, fLength); // a last line with no newline
  fLength := 0;
end;

function PeakWorkingSetMB: Double;
var
  lCounters: TProcessMemoryCounters;
begin
  FillChar(lCounters, SizeOf(lCounters), 0);
  lCounters.cb := SizeOf(lCounters);
  GetProcessMemoryInfo(GetCurrentProcess, @lCounters, SizeOf(lCounters));
  Result := lCounters.PeakWorkingSetSize / (1024 * 1024);
end;

procedure RunNDJSON(const AURL: string);
var
  lClient: THTTPClient;
  lStream: TLineStream;
  lWatch: TStopwatch;
begin
  lClient := THTTPClient.Create;
  lStream := TLineStream.Create;
  try
    lClient.ResponseTimeout := 10 * 60 * 1000;
    lClient.CustomHeaders['Accept'] := 'application/x-ndjson';
    lWatch := TStopwatch.StartNew;
    lClient.Get(AURL, lStream);
    lStream.Finish;
    lWatch.Stop;
    Writeln(Format('ndjson: %d rows, age sum %d, %d ms, %.0f rows/s, peak memory %.1f MB',
      [lStream.Rows, lStream.AgeSum, lWatch.ElapsedMilliseconds,
       lStream.Rows / (lWatch.ElapsedMilliseconds / 1000), PeakWorkingSetMB]));
  finally
    lStream.Free;
    lClient.Free;
  end;
end;

procedure RunArray(const AURL: string);
var
  lClient: THTTPClient;
  lBody: TMemoryStream;
  lJSON: TJsonArray;
  lWatch: TStopwatch;
  lAgeSum: Int64;
  I: Integer;
begin
  lClient := THTTPClient.Create;
  lBody := TMemoryStream.Create;
  try
    lClient.ResponseTimeout := 10 * 60 * 1000;
    lClient.CustomHeaders['Accept'] := 'application/json';
    lWatch := TStopwatch.StartNew;
    lClient.Get(AURL, lBody);
    // the array can only be parsed once it is complete
    lBody.Position := 0;
    lJSON := TJsonBaseObject.ParseFromStream(lBody) as TJsonArray;
    try
      lAgeSum := 0;
      for I := 0 to lJSON.Count - 1 do
        Inc(lAgeSum, lJSON.O[I].I['age']);
      lWatch.Stop;
      Writeln(Format('array:  %d rows, age sum %d, %d ms, %.0f rows/s, peak memory %.1f MB',
        [lJSON.Count, lAgeSum, lWatch.ElapsedMilliseconds,
         lJSON.Count / (lWatch.ElapsedMilliseconds / 1000), PeakWorkingSetMB]));
    finally
      lJSON.Free;
    end;
  finally
    lBody.Free;
    lClient.Free;
  end;
end;

var
  lMode, lURL: string;
begin
  lMode := LowerCase(ParamStr(1));
  lURL := ParamStr(2);
  if lURL = '' then
    lURL := 'http://localhost:8991/stream/items';
  try
    if lMode = 'array' then
      RunArray(lURL)
    else if (lMode = 'ndjson') or (lMode = '') then
      RunNDJSON(lURL)
    else
      Writeln('usage: StreamClient.exe [ndjson|array] [url]');
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
