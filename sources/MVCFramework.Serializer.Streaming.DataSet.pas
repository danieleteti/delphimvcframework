unit MVCFramework.Serializer.Streaming.DataSet;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils, Data.DB,
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type
  /// <summary>
  /// Streams a forward-only TDataSet to the client as a single JSON array,
  /// record by record, with flat server RAM (one record string + a fixed
  /// flush buffer). Owns the dataset by default. Return it from a functional
  /// action; the engine renders it via the active backend's chunk writer.
  /// </summary>
  TMVCStreamedDataSet = class(TMVCStreamedResponse)
  private
    fDataSet: TDataSet;
    fOwnsDataSet: Boolean;
    fNameCase: TMVCNameCase;
    fIgnoredFields: TMVCIgnoredList;
  public
    constructor Create(const ADataSet: TDataSet;
      const ANameCase: TMVCNameCase = ncLowerCase;
      const AOwnsDataSet: Boolean = True;
      const AIgnoredFields: TMVCIgnoredList = nil);
    destructor Destroy; override;
    procedure StreamTo(const AWriter: IMVCChunkedResponseWriter;
      const AContext: TWebContext); override;
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects;

const
  // Accumulate serialized records up to this many UTF-16 chars before
  // flushing one chunk. Bounds RAM regardless of record count.
  FLUSH_THRESHOLD_CHARS = 64 * 1024;

constructor TMVCStreamedDataSet.Create(const ADataSet: TDataSet;
  const ANameCase: TMVCNameCase; const AOwnsDataSet: Boolean;
  const AIgnoredFields: TMVCIgnoredList);
begin
  inherited Create;
  fDataSet := ADataSet;
  fNameCase := ANameCase;
  fOwnsDataSet := AOwnsDataSet;
  fIgnoredFields := AIgnoredFields;
end;

destructor TMVCStreamedDataSet.Destroy;
begin
  if fOwnsDataSet then
    fDataSet.Free;
  inherited;
end;

procedure TMVCStreamedDataSet.StreamTo(const AWriter: IMVCChunkedResponseWriter;
  const AContext: TWebContext);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lBuff: TStringBuilder;
  lFirst: Boolean;

  procedure FlushBuffer;
  begin
    if lBuff.Length = 0 then
      Exit;
    AWriter.WriteChunk(TEncoding.UTF8.GetBytes(lBuff.ToString));
    lBuff.Clear;
  end;

begin
  AWriter.SendHeaders(TMVCMediaType.APPLICATION_JSON, TMVCCharSet.UTF_8);
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lBuff := TStringBuilder.Create;
    try
      lFirst := True;
      lBuff.Append('[');
      // Forward-only iteration: no First, no bookmarks. Honors unidirectional
      // datasets (a unidirectional FireDAC cursor raises on First/GotoBookmark).
      while not fDataSet.Eof do
      begin
        if not AWriter.Connected then
          Exit; // client gone: abort, no terminator (caller will not Finish)
        if lFirst then
          lFirst := False
        else
          lBuff.Append(',');
        lBuff.Append(lSer.SerializeDataSetRecord(fDataSet, fIgnoredFields, fNameCase, nil));
        if lBuff.Length >= FLUSH_THRESHOLD_CHARS then
          FlushBuffer;
        fDataSet.Next;
      end;
      lBuff.Append(']');
      FlushBuffer;
    finally
      lBuff.Free;
    end;
  finally
    lSer.Free;
  end;
end;

end.
