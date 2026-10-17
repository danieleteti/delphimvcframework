unit MVCFramework.Serializer.Streaming.Items;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils,
  MVCFramework, MVCFramework.Commons;

type
  /// <summary>
  /// Streams a lazily produced sequence of objects to the client, one item at
  /// a time, with flat server RAM (one serialized item + a fixed flush
  /// buffer). The items come from one of three sources: a pull function
  /// (<c>TMVCItemSource&lt;T&gt;</c>), an <c>IEnumerable&lt;T&gt;</c>, or a
  /// producer that pushes with a <c>yield</c>-like callback
  /// (<c>TMVCItemProducer&lt;T&gt;</c>). Build it with the
  /// <c>StreamItems</c> / <c>StreamEach</c> helpers of the controller.
  /// The wire format follows the Accept header: JSON array (default),
  /// JSON Lines (<c>application/x-ndjson</c>) or Server-Sent Events
  /// (<c>text/event-stream</c>).
  /// </summary>
  TMVCStreamedItems<T: class> = class(TMVCStreamedResponse)
  private type
    TFormat = (fmtJSONArray, fmtNDJSON, fmtSSE);
  private
    fNext: TMVCItemSource<T>;
    fEnumerator: IEnumerator<T>;
    fProducer: TMVCItemProducer<T>;
    fOwnsItems: Boolean;
    fOnDone: TProc;
    // per-response state, valid only inside StreamTo
    fWriter: IMVCChunkedResponseWriter;
    fBuffer: TMemoryStream;
    fFormat: TFormat;
    fStarted: Boolean;
    fAborted: Boolean;
    fCount: Integer;
    fFallbackSerializer: TObject;
    procedure Done;
    procedure PutByte(const AValue: Byte);
    procedure PutText(const AValue: AnsiString);
    procedure Flush;
    procedure EnsureStarted;
    function PullNext(out AItem: T): Boolean;
    function Emit(const AItem: T): Boolean;
  public
    constructor Create(const ANext: TMVCItemSource<T>;
      const AOwnsItems: Boolean = True; const AOnDone: TProc = nil); overload;
    constructor Create(const AItems: IEnumerable<T>;
      const AOwnsItems: Boolean = True; const AOnDone: TProc = nil); overload;
    constructor CreateProducer(const AProducer: TMVCItemProducer<T>;
      const AOwnsItems: Boolean = True; const AOnDone: TProc = nil);
    destructor Destroy; override;
    procedure StreamTo(const AWriter: IMVCChunkedResponseWriter;
      const AContext: TWebContext); override;
  end;

implementation

uses
  MVCFramework.Logger,
  MVCFramework.Serializer.Streaming,
  MVCFramework.Serializer.JsonDataObjects;

const
  // Flush one chunk once this many serialized bytes are buffered. Bounds RAM
  // regardless of how many items the source produces.
  FLUSH_THRESHOLD_BYTES = 64 * 1024;

{ TMVCStreamedItems<T> }

constructor TMVCStreamedItems<T>.Create(const ANext: TMVCItemSource<T>;
  const AOwnsItems: Boolean; const AOnDone: TProc);
begin
  inherited Create;
  fNext := ANext;
  fOwnsItems := AOwnsItems;
  fOnDone := AOnDone;
end;

constructor TMVCStreamedItems<T>.Create(const AItems: IEnumerable<T>;
  const AOwnsItems: Boolean; const AOnDone: TProc);
begin
  inherited Create;
  fEnumerator := AItems.GetEnumerator;
  fOwnsItems := AOwnsItems;
  fOnDone := AOnDone;
end;

constructor TMVCStreamedItems<T>.CreateProducer(const AProducer: TMVCItemProducer<T>;
  const AOwnsItems: Boolean; const AOnDone: TProc);
begin
  inherited Create;
  fProducer := AProducer;
  fOwnsItems := AOwnsItems;
  fOnDone := AOnDone;
end;

destructor TMVCStreamedItems<T>.Destroy;
begin
  Done;
  inherited;
end;

procedure TMVCStreamedItems<T>.Done;
var
  lOnDone: TProc;
begin
  lOnDone := fOnDone;
  fOnDone := nil;
  if Assigned(lOnDone) then
  begin
    try
      lOnDone();
    except
      on E: Exception do
        LogE('StreamItems: OnDone raised ' + E.ClassName + ': ' + E.Message);
    end;
  end;
  // release whatever the closures captured
  fNext := nil;
  fProducer := nil;
  fEnumerator := nil;
end;

procedure TMVCStreamedItems<T>.PutByte(const AValue: Byte);
begin
  fBuffer.WriteBuffer(AValue, 1);
end;

procedure TMVCStreamedItems<T>.PutText(const AValue: AnsiString);
begin
  fBuffer.WriteBuffer(PAnsiChar(AValue)^, Length(AValue));
end;

procedure TMVCStreamedItems<T>.Flush;
var
  lBytes: TBytes;
begin
  if fBuffer.Size = 0 then
    Exit;
  SetLength(lBytes, fBuffer.Size);
  Move(fBuffer.Memory^, lBytes[0], fBuffer.Size);
  fWriter.WriteChunk(lBytes);
  fBuffer.Size := 0;
end;

procedure TMVCStreamedItems<T>.EnsureStarted;
begin
  if fStarted then
    Exit;
  case fFormat of
    fmtNDJSON:
      fWriter.SendHeaders(TMVCMediaType.APPLICATION_NDJSON, TMVCCharSet.UTF_8);
    fmtSSE:
      fWriter.SendHeaders(TMVCMediaType.TEXT_EVENTSTREAM, TMVCCharSet.UTF_8);
  else
    fWriter.SendHeaders(TMVCMediaType.APPLICATION_JSON, TMVCCharSet.UTF_8);
  end;
  fStarted := True;
  fHeadersSent := True;
  if fFormat = fmtJSONArray then
    PutByte(Ord('['));
end;

function TMVCStreamedItems<T>.PullNext(out AItem: T): Boolean;
begin
  AItem := nil;
  if Assigned(fNext) then
    Result := fNext(AItem)
  else
  begin
    Result := fEnumerator.MoveNext;
    if Result then
      AItem := fEnumerator.Current;
  end;
end;

function TMVCStreamedItems<T>.Emit(const AItem: T): Boolean;
var
  lBytes: TBytes;
begin
  Result := False;
  try
    if fAborted then
      Exit;
    EnsureStarted;
    if not fWriter.Connected then
    begin
      fAborted := True; // client gone: stop pulling, no terminator
      Exit;
    end;
    case fFormat of
      fmtJSONArray:
        if fCount > 0 then
          PutByte(Ord(','));
      fmtSSE:
        PutText('data: ');
    end;
    if not TMVCStreamingJsonSerializer.TryWriteObject(AItem, fBuffer) then
    begin
      // class not covered by the streaming plan: classic serializer
      if fFallbackSerializer = nil then
        fFallbackSerializer := TMVCJsonDataObjectsSerializer.Create;
      lBytes := TEncoding.UTF8.GetBytes(
        TMVCJsonDataObjectsSerializer(fFallbackSerializer).SerializeObject(AItem));
      fBuffer.WriteBuffer(lBytes[0], Length(lBytes));
    end;
    case fFormat of
      fmtNDJSON:
        PutByte(10);
      fmtSSE:
        begin
          PutByte(10);
          PutByte(10);
        end;
    end;
    Inc(fCount);
    if fBuffer.Size >= FLUSH_THRESHOLD_BYTES then
      Flush;
    Result := True;
  finally
    if fOwnsItems then
      AItem.Free;
  end;
end;

procedure TMVCStreamedItems<T>.StreamTo(const AWriter: IMVCChunkedResponseWriter;
  const AContext: TWebContext);
var
  lAccept: string;
  lItem: T;
begin
  lAccept := LowerCase(AContext.Request.Accept);
  if Pos(TMVCMediaType.APPLICATION_NDJSON, lAccept) > 0 then
    fFormat := fmtNDJSON
  else if Pos(TMVCMediaType.TEXT_EVENTSTREAM, lAccept) > 0 then
    fFormat := fmtSSE
  else
    fFormat := fmtJSONArray;
  fWriter := AWriter;
  fBuffer := TMemoryStream.Create;
  try
    if Assigned(fProducer) then
      // push: the producer calls Emit for each item and stops on False
      fProducer(Emit)
    else
    begin
      // pull: the headers go out with the first item, so a failure before it
      // is still answered with a normal error response
      while PullNext(lItem) do
        if not Emit(lItem) then
          Break;
    end;
    if not fAborted then
    begin
      EnsureStarted; // an empty source is a valid, empty response
      if fFormat = fmtJSONArray then
        PutByte(Ord(']'));
      Flush;
    end;
  finally
    fFallbackSerializer.Free;
    fFallbackSerializer := nil;
    fBuffer.Free;
    fBuffer := nil;
    fWriter := nil;
    Done;
  end;
end;

end.
