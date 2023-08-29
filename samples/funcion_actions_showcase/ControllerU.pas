unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections, Data.DB;

type
  [MVCNameCase(ncCamelCase)]
  TPersonRec = record
    FirstName, LastName: String;
    Age: Integer;
    class function Create: TPersonRec; static;
  end;

  [MVCNameCase(ncCamelCase)]
  TPerson = class
  private
    fAge: Integer;
    fFirstName, fLastName: String;
  public
    property FirstName: String read fFirstName write fFirstName;
    property LastName: String read fLastName write fLastName;
    property Age: Integer read fAge write fAge;
    constructor Create(FirstName, LastName: String; Age: Integer);
  end;

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    { actions returning a simple type }
    [MVCPath('/sumsasinteger/($A)/($B)')]
    function GetSum(const A, B: Integer): Integer;
    [MVCPath('/sumsasfloat/($A)/($B)')]
    function GetSumAsFloat(const A, B: Extended): Extended;

    { actions returning records }
    [MVCPath('/records/single')]
    function GetSingleRecord: TPersonRec;
    [MVCPath('/records/multiple')]
    function GetMultipleRecords: TArray<TPersonRec>;

    { actions returning objects }
    [MVCPath('/objects/single')]
    function GetSingleObject: TPerson;
    [MVCPath('/objects/multiple')]
    function GetMultipleObjects: TObjectList<TPerson>;

    { actions returning datasets }
    [MVCPath('/datasets/single')]
    function GetSingleDataSet: TDataSet;
    [MVCPath('/datasets/multiple')]
    function GetMultipleDataSet: TEnumerable<TDataSet>;
    [MVCPath('/datasets/multiple2')]
    function GetMultipleDataSet2: IMVCObjectDictionary;

    { customize response headers }
    [MVCPath('/headers')]
    function GetWithCustomHeaders: TObjectList<TPerson>;

    { using IMVCResponse }
    [MVCPath('/mvcresponse/message')]
    function GetMVCResponseSimple: IMVCResponse;
    [MVCPath('/mvcresponse/data')]
    function GetMVCResponseWithData: IMVCResponse;
    [MVCPath('/mvcresponse/list')]
    function GetMVCResponseWithObjectList: IMVCResponse;
    [MVCPath('/mvcresponse/dictionary')]
    function GetMVCResponseWithObjectDictionary: IMVCResponse;
    [MVCPath('/mvcresponse/error')]
    function GetMVCErrorResponse: IMVCResponse;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, System.DateUtils,
  MainDMU, FireDAC.Comp.Client, MVCFramework.FireDAC.Utils;

{ TMyController }

function TMyController.GetMultipleDataSet: TEnumerable<TDataSet>;
begin
  var lDM := TdmMain.Create(nil);
  try
    lDM.dsPeople.Open;
    var lList := TObjectList<TDataSet>.Create;
    lList.Add(TFDMemTable.CloneFrom(lDM.dsPeople));
    lList.Add(TFDMemTable.CloneFrom(lDM.dsPeople));
    Result := lList;
  finally
    lDM.Free;
  end;
end;

function TMyController.GetMultipleDataSet2: IMVCObjectDictionary;
begin
  var lDM := TdmMain.Create(nil);
  try
    lDM.dsPeople.Open;
    Result := ObjectDict()
      .Add('people1', TFDMemTable.CloneFrom(lDM.dsPeople))
      .Add('people2', TFDMemTable.CloneFrom(lDM.dsPeople));
  finally
    lDM.Free;
  end;
end;

function TMyController.GetMultipleObjects: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;

  Result.Add(TPerson.Create('Daniele', 'Teti', YearsBetween(Date, EncodeDateDay(1979, 1))));

  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 10));

  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 20));
end;

function TMyController.GetMultipleRecords: TArray<TPersonRec>;
begin
  SetLength(Result, 3);
  Result[0] := TPersonRec.Create;
  Result[1] := TPersonRec.Create;
  Result[2] := TPersonRec.Create;

  Inc(Result[1].Age, 10);

  Inc(Result[2].Age, 20);
end;

function TMyController.GetMVCErrorResponse: IMVCResponse;
begin
  Result := TMVCErrorResponse.Create(500, 'boom');
end;

function TMyController.GetMVCResponseSimple: IMVCResponse;
begin
  Result := MVCResponse(HTTP_STATUS.OK, 'My Message', 'My Reason String');
end;

function TMyController.GetMVCResponseWithData: IMVCResponse;
begin
  Result := MVCResponse(HTTP_STATUS.OK, TPerson.Create('Daniele','Teti', 99));
end;

function TMyController.GetMVCResponseWithObjectDictionary: IMVCResponse;
begin
  Result := MVCResponse(
    HTTP_STATUS.OK,
    ObjectDict()
      .Add('people1', TObjectList<TPerson>.Create([
                      TPerson.Create('Daniele','Teti', 99),
                      TPerson.Create('Peter','Parker', 25),
                      TPerson.Create('Bruce','Banner', 45)
                    ])
      )
      .Add('people2', TObjectList<TPerson>.Create([
                      TPerson.Create('Daniele','Teti', 99),
                      TPerson.Create('Peter','Parker', 25),
                      TPerson.Create('Bruce','Banner', 45)
                    ])
      )
  );
end;

function TMyController.GetMVCResponseWithObjectList: IMVCResponse;
begin
  Result := MVCResponse(
    HTTP_STATUS.OK,
    TObjectList<TPerson>.Create([
      TPerson.Create('Daniele','Teti', 99),
      TPerson.Create('Peter','Parker', 25),
      TPerson.Create('Bruce','Banner', 45)
    ])
  );
end;

function TMyController.GetSingleDataSet: TDataSet;
begin
  var lDM := TdmMain.Create(nil);
  try
    lDM.dsPeople.Open;
    Result := TFDMemTable.CloneFrom(lDM.dsPeople);
  finally
    lDM.Free;
  end;
end;

function TMyController.GetSingleObject: TPerson;
begin
  Result := TPerson.Create('Daniele', 'Teti', YearsBetween(Date, EncodeDateDay(1979, 1)));
end;

function TMyController.GetSingleRecord: TPersonRec;
begin
  Result := TPersonRec.Create;
end;

function TMyController.GetSum(const A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TMyController.GetSumAsFloat(const A, B: Extended): Extended;
begin
  Result := A + B;
end;

function TMyController.GetWithCustomHeaders: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;

  Result.Add(TPerson.Create('Daniele', 'Teti', YearsBetween(Date, EncodeDateDay(1979, 1))));

  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 10));

  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 20));

  { customize headers }
  Context.Response.StatusCode := HTTP_STATUS.PartialContent;
  Context.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
  Context.Response.SetCustomHeader('X-MYHEADER', 'HELLO WORLD');
end;

{ TPersonRec }

class function TPersonRec.Create: TPersonRec;
begin
  Result.FirstName := 'Daniele';
  Result.LastName := 'Teti';
  Result.Age := YearsBetween(Date, EncodeDateDay(1979, 1));
end;

{ TPerson }

constructor TPerson.Create(FirstName, LastName: String; Age: Integer);
begin
  inherited Create;
  fFirstName := FirstName;
  fLastName := LastName;
  fAge := Age;
end;

end.
