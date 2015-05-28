unit Iocp.Buffer;

interface

uses
  Windows, Classes, SysUtils, SyncObjs;

type
  TCircularBuffer = class
  private
    FBuffer: PByteArray;
    FSize: Integer;
    FWritePos: Integer;
    FReadPos: Integer;
    FLocker: TCriticalSection;

    function GetPtr(Index: Integer): Pointer; //inline;
    procedure Grow;

    function GetMaxReadBytes: Integer;
    function GetMaxWriteBytes: Integer;
    procedure SetSize(NewSize: Integer);
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
    function Write(const Buf; Size: Integer): Boolean;
    function Read(var Buf; Size: Integer): Boolean;
    procedure Clear;

    property WritePosition: Integer read FWritePos write FWritePos;
    property ReadPosition: Integer read FReadPos write FReadPos;
    property Size: Integer read FSize write SetSize;
    property ReadAvail: Integer read GetMaxReadBytes;
    property WriteAvail: Integer read GetMaxWriteBytes;
    property Bytes[Index: Integer]: Pointer read GetPtr; default;
  end;

  TIocpStringStream = class(TStream)
  private
    FDataString: RawByteString;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: RawByteString);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): RawByteString;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Clear;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: string); overload;
    procedure WriteString(const AString: RawByteString); overload;
    procedure WriteString(const AString: AnsiString); overload;
    property DataString: RawByteString read FDataString;
  end;

implementation

uses
  Math;

constructor TCircularBuffer.Create(Size: Integer);
begin
  inherited Create;

  FSize := Size;
  FBuffer := AllocMem(Size);
  FLocker := TCriticalSection.Create;
end;

destructor TCircularBuffer.Destroy;
begin
  FreeMem(FBuffer);
  FLocker.Free;

  inherited Destroy;
end;

procedure TCircularBuffer.Clear;
begin
  Lock;
  FReadPos := 0;
  FWritePos := 0;
  Unlock;
end;

function TCircularBuffer.GetMaxReadBytes: Integer;
begin
  Lock;
  if FReadPos = FWritePos then
  begin
    Result := 0;
  end else if FReadPos < FWritePos then
  begin
    Result := FWritePos - FReadPos;
  end else
  begin
    Result := (FSize - FReadPos) + FWritePos;
  end;
  Unlock;
end;

function TCircularBuffer.GetMaxWriteBytes: Integer;
begin
  Lock;
  if FReadPos = FWritePos then
  begin
    Result := FSize;
  end else if FWritePos < FReadPos then
  begin
    Result := FReadPos - FWritePos;
  end else
  begin
    Result := (FSize - FWritePos) + FReadPos;
  end;
  Unlock;
end;

function TCircularBuffer.GetPtr(Index: Integer): Pointer;
begin
  Result := @FBuffer^[Index];
end;

procedure TCircularBuffer.Grow;
begin
  Inc(FSize, FSize shr 1);
  ReallocMem(FBuffer, FSize);
end;

procedure TCircularBuffer.Lock;
begin
  FLocker.Enter;
end;

function TCircularBuffer.Read(var Buf; Size: Integer): Boolean;
var
  S1, S2: Integer;
begin
  Lock;
  try
    if Size > GetMaxReadBytes then
    begin
      Result := False;
      Exit;
    end;

    if FReadPos + Size <= FSize then
    begin
      { No wrapping }
      Move(FBuffer^[FReadPos], Buf, Size);
      Inc(FReadPos, Size);
    end else
    begin
      { With wrapping }
      S1 := FSize - FReadPos;
      S2 := Size - S1;

      Move(FBuffer^[FReadPos], Buf, S1);
      Move(FBuffer^[0], PByteArray(@Buf)^[S1], S2);

      FReadPos := S2;
      Grow; { To reduce future wrapping }
    end;
  finally
    Unlock;
  end;
  Result := True;
end;

function TCircularBuffer.Write(const Buf; Size: Integer): Boolean;
var
  S1, S2: Integer;
begin
  Lock;
  try
    if Size >= GetMaxWriteBytes then
    begin
      Result := False;
      Exit;
    end;

    if FWritePos + Size < FSize then
    begin
      { No wrapping }
      Move(Buf, FBuffer^[FWritePos], Size);
      Inc(FWritePos, Size);
    end else
    begin
      { With wrapping }
      S1 := FSize - FWritePos;
      S2 := Size - S1;

      Move(Buf, FBuffer^[FWritePos], S1);
      Move(PByteArray(@Buf)^[S1], FBuffer^[0], S2);

      FWritePos := S2;
      Grow; { To reduce future wrapping }
    end;
  finally
    Unlock;
  end;
  Result := True;
end;

procedure TCircularBuffer.SetSize(NewSize: Integer);
begin
  Lock;
  try
    if NewSize <> FSize then
    begin
      ReallocMem(FBuffer, NewSize);
      FSize := NewSize;
    end;
  finally
    Unlock;
  end;
end;

procedure TCircularBuffer.Unlock;
begin
  FLocker.Leave;
end;

{ TIocpStringStream }

procedure TIocpStringStream.Clear;
begin
  Size := 0;
end;

constructor TIocpStringStream.Create(const AString: RawByteString);
begin
  inherited Create;
  FDataString := AString;
end;

destructor TIocpStringStream.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TIocpStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Buffer, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TIocpStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TIocpStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TIocpStringStream.ReadString(Count: Longint): RawByteString;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetLength(Result, Len);
  Move(PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, PAnsiChar(Result)^, Len * SizeOf(AnsiChar));
//  SetString(Result, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)]), Len);
  Inc(FPosition, Len);
end;

procedure TIocpStringStream.WriteString(const AString: string);
var
  RawData: RawByteString;
begin
  RawData := RawByteString(AString);
  Write(PAnsiChar(RawData)^, Length(RawData) * SizeOf(AnsiChar));
end;

procedure TIocpStringStream.WriteString(const AString: RawByteString);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

procedure TIocpStringStream.WriteString(const AString: AnsiString);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

procedure TIocpStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

end.
