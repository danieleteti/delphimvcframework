unit EnumSerializer;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Commons;

type

  [TestFixture]
  TMVCTestSerializerEnums = class(TObject)
  private

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSerializeEnum;
    [Test]
    procedure TestSerializeEnumOrd;
    [Test]
    procedure TestSerializeEnumName;
  end;

  TMonthEnum = (meJanuary, meFebruary, meMarch, meApril);

  TEntityWithEnums = class
  private
    FMonth: TMonthEnum;
    FMonthName: TMonthEnum;
    FMonthOrder: TMonthEnum;
    FMonthName2: TMonthEnum;
  public
    property Month: TMonthEnum read FMonth write FMonth;
    [MVCEnumSerialization(estEnumMappedValues, 'January,February,March,April')]
    property MonthName: TMonthEnum read FMonthName write FMonthName;
    [MVCEnumSerialization(estEnumName)]
    property MonthName2: TMonthEnum read FMonthName2 write FMonthName2;
    [MVCEnumSerialization(estEnumOrd)]
    property MonthOrder: TMonthEnum read FMonthOrder write FMonthOrder;
  end;

implementation

{ TMVCTestSerializerEnums }

procedure TMVCTestSerializerEnums.Setup;
begin

end;

procedure TMVCTestSerializerEnums.TearDown;
begin
  inherited;
end;

procedure TMVCTestSerializerEnums.TestSerializeEnum;
const
  JSON = '{' + '"Month":"meJanuary",' + '"MonthName":"January",' + '"MonthName2":"meFebruary",' +
    '"MonthOrder":0' + '}';
var
  O: TEntityWithEnums;
  S: string;
  Serializer: IMVCSerializer;
begin
  Serializer := TMVCJsonDataObjectsSerializer.Create();
  try
    O := TEntityWithEnums.Create;
    try
      O.Month := TMonthEnum.meJanuary;
      O.MonthName := TMonthEnum.meJanuary;
      O.MonthName2 := TMonthEnum.meFebruary;
      O.MonthOrder := TMonthEnum.meJanuary;
      S := Serializer.SerializeObject(O);
      Assert.areEqual(JSON, S);
    finally
      O.Free;
    end;

    O := TEntityWithEnums.Create;
    try
      Serializer.DeserializeObject(S, O);
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.Month));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthName));
      Assert.areEqual(Ord(TMonthEnum.meFebruary), Ord(O.MonthName2));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthOrder));
    finally
      O.Free;
    end;
  finally
    Serializer := nil;
  end;
end;

procedure TMVCTestSerializerEnums.TestSerializeEnumName;
const
  JSON = '{' + '"Month":"meJanuary",' + '"MonthName":"January",' + '"MonthName2":"meFebruary",' +
    '"MonthOrder":0' + '}';
var
  O: TEntityWithEnums;
  S: string;
  Serializer: IMVCSerializer;
  Config: TMVCConfig;
begin
  Config := TMVCConfig.Create;
  Config[TMVCConfigKey.EnumSerializationType] := 'estEnumName';
  Serializer := TMVCJsonDataObjectsSerializer.Create(Config);
  try
    O := TEntityWithEnums.Create;
    try
      O.Month := TMonthEnum.meJanuary;
      O.MonthName := TMonthEnum.meJanuary;
      O.MonthName2 := TMonthEnum.meFebruary;
      O.MonthOrder := TMonthEnum.meJanuary;
      S := Serializer.SerializeObject(O);
      Assert.areEqual(JSON, S);
    finally
      O.Free;
    end;

    O := TEntityWithEnums.Create;
    try
      Serializer.DeserializeObject(S, O);
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.Month));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthName));
      Assert.areEqual(Ord(TMonthEnum.meFebruary), Ord(O.MonthName2));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthOrder));
    finally
      O.Free;
    end;
  finally
    Config.Free;
    Serializer := nil;
  end;
end;

procedure TMVCTestSerializerEnums.TestSerializeEnumOrd;
const
  JSON = '{' + '"Month":0,' + '"MonthName":"January",' + '"MonthName2":"meFebruary",' +
    '"MonthOrder":0' + '}';
var
  O: TEntityWithEnums;
  S: string;
  Serializer: IMVCSerializer;
  Config: TMVCConfig;
begin
  Config := TMVCConfig.Create;
  Config[TMVCConfigKey.EnumSerializationType] := 'estEnumOrd';
  Serializer := TMVCJsonDataObjectsSerializer.Create(Config);
  try
    O := TEntityWithEnums.Create;
    try
      O.Month := TMonthEnum.meJanuary;
      O.MonthName := TMonthEnum.meJanuary;
      O.MonthName2 := TMonthEnum.meFebruary;
      O.MonthOrder := TMonthEnum.meJanuary;
      S := Serializer.SerializeObject(O);
      Assert.areEqual(JSON, S);
    finally
      O.Free;
    end;

    O := TEntityWithEnums.Create;
    try
      Serializer.DeserializeObject(S, O);
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.Month));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthName));
      Assert.areEqual(Ord(TMonthEnum.meFebruary), Ord(O.MonthName2));
      Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthOrder));
    finally
      O.Free;
    end;
  finally
    Config.Free;
    Serializer := nil;
  end;
end;

end.
