unit StreamingControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  [MVCPath('/api')]
  TStreamingController = class(TMVCController)
  public
    /// <summary>
    /// SSE: Simulates AI chat streaming - sends text token by token.
    /// Connect with EventSource: new EventSource('/api/chat')
    /// </summary>
    [MVCPath('/chat')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure ChatStream;

    /// <summary>
    /// SSE: Streams progress updates for a long-running task.
    /// Connect with EventSource: new EventSource('/api/progress')
    /// </summary>
    [MVCPath('/progress')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure ProgressStream;

    /// <summary>
    /// JSONL: Streams a large dataset as newline-delimited JSON.
    /// Consume with fetch() and read line by line.
    /// </summary>
    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/x-ndjson')]
    procedure PeopleStream;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, MVCFramework.SSE.Writer;

procedure TStreamingController.ChatStream;
const
  RESPONSE_TEXT = 'DelphiMVCFramework now supports Server-Sent Events streaming! ' +
    'This response is being sent token by token, just like an AI chat. ' +
    'Each word is a separate SSE event with a small delay to simulate generation.';
var
  lSSE: TMVCSSEWriter;
  lWords: TArray<string>;
  I: Integer;
begin
  lSSE := TMVCSSEWriter.Create(Context);
  try
    lWords := RESPONSE_TEXT.Split([' ']);
    for I := 0 to High(lWords) do
    begin
      if not lSSE.Connected then
        Break;
      lSSE.Send('token', lWords[I], IntToStr(I));
      Sleep(80 + Random(120)); // simulate generation delay
    end;
    if lSSE.Connected then
      lSSE.Send('done', '{"tokens": ' + IntToStr(Length(lWords)) + '}');
  finally
    lSSE.Free;
  end;
end;

procedure TStreamingController.ProgressStream;
var
  lSSE: TMVCSSEWriter;
  I: Integer;
begin
  lSSE := TMVCSSEWriter.Create(Context);
  try
    lSSE.Send('status', '{"phase": "starting", "percent": 0}');
    for I := 1 to 10 do
    begin
      if not lSSE.Connected then
        Break;
      Sleep(300 + Random(500)); // simulate work
      lSSE.Send('progress', Format('{"phase": "processing", "percent": %d, "step": %d}', [I * 10, I]));
    end;
    if lSSE.Connected then
      lSSE.Send('complete', '{"phase": "done", "percent": 100}');
  finally
    lSSE.Free;
  end;
end;

procedure TStreamingController.PeopleStream;
const
  FIRST_NAMES: array[0..9] of string = (
    'Peter', 'Bruce', 'Reed', 'Tony', 'Natasha',
    'Steve', 'Wanda', 'Scott', 'Carol', 'Clint');
  LAST_NAMES: array[0..9] of string = (
    'Parker', 'Banner', 'Richards', 'Stark', 'Romanoff',
    'Rogers', 'Maximoff', 'Lang', 'Danvers', 'Barton');
var
  lJSONL: TMVCJSONLWriter;
  I: Integer;
begin
  lJSONL := TMVCJSONLWriter.Create(Context);
  try
    for I := 1 to 100 do
    begin
      if not lJSONL.Connected then
        Break;
      lJSONL.Send(Format('{"id":%d,"firstName":"%s","lastName":"%s","age":%d}',
        [I, FIRST_NAMES[Random(10)], LAST_NAMES[Random(10)], 20 + Random(50)]));
      if I mod 10 = 0 then
        Sleep(100); // simulate batch processing
    end;
  finally
    lJSONL.Free;
  end;
end;

end.
