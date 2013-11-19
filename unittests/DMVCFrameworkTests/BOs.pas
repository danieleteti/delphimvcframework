unit BOs;

interface

uses
  FrameworkTestsU, system.TimeSpan, system.SysUtils;

type
  TMyObject = class
  private
    FPropString: string;
    FPropAnsiString: AnsiString;
    FPropInt64: Int64;
    FPropUInt32: cardinal;
    FPropUInt64: UInt64;
    FPropUInt16: word;
    FPropInt16: smallint;
    FPropBoolean: boolean;
    FPropDateTime: TDateTime;
    FPropDate: TDate;
    FPropInteger: Integer;
    FPropTimeStamp: TTimeStamp;
    FPropTime: TTime;
    procedure SetPropAnsiString(const Value: AnsiString);
    procedure SetPropString(const Value: string);
    procedure SetPropInt64(const Value: Int64);
    procedure SetPropUInt32(const Value: cardinal);
    procedure SetPropUInt64(const Value: UInt64);
    procedure SetPropInt16(const Value: smallint);
    procedure SetPropUInt16(const Value: word);
    procedure SetPropBoolean(const Value: boolean);
    procedure SetPropDate(const Value: TDate);
    procedure SetPropDateTime(const Value: TDateTime);
    procedure SetPropInteger(const Value: Integer);
    procedure SetPropTimeStamp(const Value: TTimeStamp);
    procedure SetPropTime(const Value: TTime);
  public
    function Equals(Obj: TMyObject): boolean;
    property PropString: string read FPropString write SetPropString;
    property PropAnsiString: AnsiString read FPropAnsiString write SetPropAnsiString;
    property PropInteger: Integer read FPropInteger write SetPropInteger;
    property PropUInt32: cardinal read FPropUInt32 write SetPropUInt32;
    property PropInt64: Int64 read FPropInt64 write SetPropInt64;
    property PropUInt64: UInt64 read FPropUInt64 write SetPropUInt64;
    property PropUInt16: word read FPropUInt16 write SetPropUInt16;
    property PropInt16: smallint read FPropInt16 write SetPropInt16;
    property PropBoolean: boolean read FPropBoolean write SetPropBoolean;
    property PropDate: TDate read FPropDate write SetPropDate;
    property PropTime: TTime read FPropTime write SetPropTime;
    property PropDateTime: TDateTime read FPropDateTime write SetPropDateTime;
    property PropTimeStamp: TTimeStamp read FPropTimeStamp write SetPropTimeStamp;
  end;

function GetMyObject: TMyObject;

implementation

uses
  system.DateUtils;

function GetMyObject: TMyObject;
begin
  Result := TMyObject.Create;
  Result.PropString := 'Some text אטילעש';
  Result.PropAnsiString := 'This is an ANSI text';
  Result.PropInteger := -1234;
  Result.PropUInt32 := 1234;
  Result.PropInt64 := -1234567890;
  Result.PropUInt64 := 1234567890;
  Result.PropUInt16 := 12345;
  Result.PropInt16 := -12345;
  Result.PropBoolean := true;
  Result.PropDate := EncodeDate(2010, 10, 20);
  Result.PropTime := EncodeTime(10, 20, 30, 40);
  Result.PropDateTime := Result.PropDate + Result.PropTime;
  Result.PropTimeStamp := DateTimeToTimeStamp(Result.PropDateTime + 1);
end;

function TMyObject.Equals(Obj: TMyObject): boolean;
begin
  Result := true;
  Result := Result and (Self.PropString = Obj.PropString);
  Result := Result and (Self.PropAnsiString = Obj.PropAnsiString);
  Result := Result and (Self.PropInteger = Obj.PropInteger);
  Result := Result and (Self.PropUInt32 = Obj.PropUInt32);
  Result := Result and (Self.PropInt64 = Obj.PropInt64);
  Result := Result and (Self.PropUInt64 = Obj.PropUInt64);
  Result := Result and (Self.PropUInt16 = Obj.PropUInt16);
  Result := Result and (Self.PropInt16 = Obj.PropInt16);
  Result := Result and (Self.PropBoolean = Obj.PropBoolean);
  Result := Result and (Self.PropDate = Obj.PropDate);
  Result := Result and (SecondsBetween(Self.PropTime, Obj.PropTime) = 0);
  Result := Result and (SecondsBetween(Self.PropDateTime, Obj.PropDateTime) = 0);
  Result := Result and (Self.PropTimeStamp.Date = Obj.PropTimeStamp.Date) and
    (Self.PropTimeStamp.Time = Obj.PropTimeStamp.Time);
end;

procedure TMyObject.SetPropAnsiString(const Value: AnsiString);
begin
  FPropAnsiString := Value;
end;

procedure TMyObject.SetPropBoolean(const Value: boolean);
begin
  FPropBoolean := Value;
end;

procedure TMyObject.SetPropDate(const Value: TDate);
begin
  FPropDate := Value;
end;

procedure TMyObject.SetPropDateTime(const Value: TDateTime);
begin
  FPropDateTime := Value;
end;

procedure TMyObject.SetPropInt16(const Value: smallint);
begin
  FPropInt16 := Value;
end;

procedure TMyObject.SetPropInt64(const Value: Int64);
begin
  FPropInt64 := Value;
end;

procedure TMyObject.SetPropInteger(const Value: Integer);
begin
  FPropInteger := Value;
end;

procedure TMyObject.SetPropString(const Value: string);
begin
  FPropString := Value;
end;

procedure TMyObject.SetPropTime(const Value: TTime);
begin
  FPropTime := Value;
end;

procedure TMyObject.SetPropTimeStamp(const Value: TTimeStamp);
begin
  FPropTimeStamp := Value;
end;

procedure TMyObject.SetPropUInt16(const Value: word);
begin
  FPropUInt16 := Value;
end;

procedure TMyObject.SetPropUInt32(const Value: cardinal);
begin
  FPropUInt32 := Value;
end;

procedure TMyObject.SetPropUInt64(const Value: UInt64);
begin
  FPropUInt64 := Value;
end;

end.
