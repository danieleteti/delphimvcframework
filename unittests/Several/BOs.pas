unit BOs;

interface

uses
  FrameworkTestsU, system.TimeSpan, system.SysUtils, generics.collections,
  ObjectsMappers, system.Classes;

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
    FPropCurrency: Currency;
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
    procedure SetPropCurrency(const Value: Currency);
  public
    function Equals(Obj: TMyObject): boolean; reintroduce;
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
    property PropCurrency: Currency read FPropCurrency write SetPropCurrency;
  end;

  TMyChildObject = class
  private
    FMyChildProperty1: string;
    procedure SetMyChildProperty1(const Value: string);
  public
    constructor Create;
    property MyChildProperty1: string read FMyChildProperty1 write SetMyChildProperty1;
  end;

  [MapperListOf(TMyChildObject)]
  TMyChildObjectList = class(TObjectList<TMyChildObject>)
  end;

  TMyComplexObject = class
  private
    FProp1: string;
    FChildObjectList: TMyChildObjectList;
    FChildObject: TMyChildObject;
    procedure SetChildObject(const Value: TMyChildObject);
    procedure SetChildObjectList(const Value: TMyChildObjectList);
    procedure SetProp1(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;

    property Prop1: string read FProp1 write SetProp1;
    property ChildObject: TMyChildObject read FChildObject write SetChildObject;
    property ChildObjectList: TMyChildObjectList read FChildObjectList write SetChildObjectList;
  end;

  TMyStreamObject = class(TObject)
  private
    FPropStream: TStream;
    FProp8Stream: TStream;
    procedure SetPropStream(const Value: TStream);
    procedure SetProp8Stream(const Value: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    [MapperSerializeAsString('utf-16')]
    property PropStream: TStream read FPropStream write SetPropStream;
    [MapperSerializeAsString]
    // utf-8 is default
    property Prop8Stream: TStream read FProp8Stream write SetProp8Stream;
  end;

  TMyObjectWithLogic = class
  private
    FLastName: string;
    FFirstName: string;
    FAge: Integer;
    function GetFullName: string;
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function GetIsAdult: boolean;
    procedure SetAge(const Value: Integer);
  public
    constructor Create(aFirstName, aLastName: string; aAge: Integer);
    function Equals(Obj: TObject): boolean; override;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property IsAdult: boolean read GetIsAdult;
    property FullName: string read GetFullName;
  end;

function GetMyObject: TMyObject;
function GetMyComplexObject: TMyComplexObject;
function GetMyComplexObjectWithNotInitializedChilds: TMyComplexObject;

implementation

uses
  system.DateUtils;

function GetMyComplexObjectWithNotInitializedChilds: TMyComplexObject;
begin
  Result := TMyComplexObject.Create;
  Result.Prop1 := 'property1';
end;

function GetMyComplexObject: TMyComplexObject;
var
  co: TMyChildObject;
begin
  Result := TMyComplexObject.Create;
  Result.Prop1 := 'property1';
  Result.ChildObject.MyChildProperty1 := 'MySingleChildProperty1';
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty1';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty2';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty3';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty4';
  Result.ChildObjectList.Add(co);
end;

function GetMyObject: TMyObject;
begin
  Result := TMyObject.Create;
  Result.PropString := 'Some text ������';
  Result.PropAnsiString := 'This is an ANSI text';
  Result.PropInteger := - 1234;
  Result.PropUInt32 := 1234;
  Result.PropInt64 := - 1234567890;
  Result.PropUInt64 := 1234567890;
  Result.PropUInt16 := 12345;
  Result.PropInt16 := - 12345;
  Result.PropCurrency := 1234.5678;
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
  Result := Result and (Self.PropCurrency = Obj.PropCurrency);
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

procedure TMyObject.SetPropCurrency(const Value: Currency);
begin
  FPropCurrency := Value;
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

{ TMyComplexObject }

constructor TMyComplexObject.Create;
begin
  inherited;
  FChildObjectList := TMyChildObjectList.Create(true);
  FChildObject := TMyChildObject.Create;
end;

destructor TMyComplexObject.Destroy;
begin
  FChildObjectList.Free;
  FChildObject.Free;
  inherited;
end;

function TMyComplexObject.Equals(Obj: TObject): boolean;
var
  co: TMyComplexObject;
begin
  co := Obj as TMyComplexObject;

  Result := co.Prop1 = Self.Prop1;
  if Assigned(co.ChildObject) and Assigned(Self.ChildObject) then
    Result := Result and (co.ChildObject.MyChildProperty1 = Self.ChildObject.MyChildProperty1)
  else
    Result := Result and (not Assigned(co.ChildObject)) and (not Assigned(Self.ChildObject));
  Result := Result and (co.ChildObjectList.Count = Self.ChildObjectList.Count);
  if co.ChildObjectList.Count = 0 then
    Exit;

  Result := Result and (co.ChildObjectList[0].MyChildProperty1 = Self.ChildObjectList[0]
    .MyChildProperty1);
  Result := Result and (co.ChildObjectList[1].MyChildProperty1 = Self.ChildObjectList[1]
    .MyChildProperty1);
  Result := Result and (co.ChildObjectList[2].MyChildProperty1 = Self.ChildObjectList[2]
    .MyChildProperty1);
  Result := Result and (co.ChildObjectList[3].MyChildProperty1 = Self.ChildObjectList[3]
    .MyChildProperty1);
end;

procedure TMyComplexObject.SetChildObject(const Value: TMyChildObject);
begin
  FChildObject := Value;
end;

procedure TMyComplexObject.SetChildObjectList(const Value: TMyChildObjectList);
begin
  FChildObjectList := Value;
end;

procedure TMyComplexObject.SetProp1(const Value: string);
begin
  FProp1 := Value;
end;

{ TMyChildObject }

constructor TMyChildObject.Create;
begin
  inherited;
  Self.MyChildProperty1 := 'my default value';
end;

procedure TMyChildObject.SetMyChildProperty1(const Value: string);
begin
  FMyChildProperty1 := Value;
end;

{ TMyStreamObject }

constructor TMyStreamObject.Create;
begin
  inherited Create;
  FPropStream := TStringStream.Create('', TEncoding.Unicode);
  FProp8Stream := TStringStream.Create('', TEncoding.UTF8);
end;

destructor TMyStreamObject.Destroy;
begin
  if Assigned(FPropStream) then
    FPropStream.Free;
  if Assigned(FProp8Stream) then
    FProp8Stream.Free;
  inherited;
end;

procedure TMyStreamObject.SetProp8Stream(const Value: TStream);
begin
  if Assigned(FProp8Stream) then
    FProp8Stream.Free;
  FProp8Stream := Value;
end;

procedure TMyStreamObject.SetPropStream(const Value: TStream);
begin
  if Assigned(FPropStream) then
    FPropStream.Free;
  FPropStream := Value;
end;

{ TCliente }

{ TMyObjectWithLogic }

constructor TMyObjectWithLogic.Create(aFirstName, aLastName: string; aAge: Integer);
begin
  inherited Create;
  FFirstName := aFirstName;
  FLastName := aLastName;
  FAge := aAge;
end;

function TMyObjectWithLogic.Equals(Obj: TObject): boolean;
var
  lOther: TMyObjectWithLogic;
begin
  Result := (Obj is TMyObjectWithLogic);
  if Result then
  begin
    lOther := TMyObjectWithLogic(Obj);
    Result := Result and (Self.FirstName = lOther.FirstName);
    Result := Result and (Self.LastName = lOther.LastName);
    Result := Result and (Self.Age = lOther.Age);
  end;
end;

function TMyObjectWithLogic.GetFullName: string;
begin
  Result := FirstName + ' ' + LastName; // logic
end;

function TMyObjectWithLogic.GetIsAdult: boolean;
begin
  Result := Age >= 18; // logic
end;

procedure TMyObjectWithLogic.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TMyObjectWithLogic.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TMyObjectWithLogic.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

end.
