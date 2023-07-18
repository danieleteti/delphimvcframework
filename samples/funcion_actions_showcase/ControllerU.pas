unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections;

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

    { customize response headers }
    [MVCPath('/headers')]
    function GetWithCustomHeaders: TObjectList<TPerson>;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, System.DateUtils;

{ TMyController }

function TMyController.GetMultipleObjects: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := YearsBetween(Date, EncodeDateDay(1979, 1));

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := Result[0].Age + 10;

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := Result[0].Age + 20;
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

function TMyController.GetSingleObject: TPerson;
begin
  Result := TPerson.Create;
  Result.FirstName := 'Daniele';
  Result.LastName := 'Teti';
  Result.Age := YearsBetween(Date, EncodeDateDay(1979, 1));
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

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := YearsBetween(Date, EncodeDateDay(1979, 1));

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := Result[0].Age + 10;

  Result.Add(TPerson.Create);
  Result.Last.FirstName := 'Daniele';
  Result.Last.LastName := 'Teti';
  Result.Last.Age := Result[0].Age + 20;

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

end.
