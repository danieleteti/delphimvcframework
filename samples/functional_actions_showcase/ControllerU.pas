unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections, Data.DB, JsonDataObjects, System.Rtti,
  System.Classes;

type
  TPersonRec = record
    FirstName, LastName: String;
    Age: Integer;
    class function Create: TPersonRec; static;
  end;

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
    [MVCPath('/booleans/($A)/($B)')]
    function GetOrTruthTable(const A, B: Boolean): Boolean;
    [MVCPath('/strings/($A)/($B)')]
    function GetConcatAsString(const A, B: String): String;

    { actions returning records }
    [MVCPath('/records/single')]
    function GetSingleRecord: TPersonRec;
    [MVCPath('/records/multiple')]
    function GetMultipleRecords: TArray<TPersonRec>;

    { actions returning objects and binary data}
    [MVCPath('/objects/single')]
    function GetSingleObject: TPerson;
    [MVCPath('/objects/multiple')]
    function GetMultipleObjects: TObjectList<TPerson>;
    [MVCPath('/files/customers/($ID)')]
    function GetCustomerPhoto(const ID: Integer): TStream;
    [MVCPath('/files/sea/($ID)')]
    function GetSeaPhoto(const ID: Integer): TStream;

    { actions returning json }
    [MVCPath('/objects/jsonobject')]
    function GetJSONObject: TJSONObject;
    [MVCPath('/objects/jsonarray')]
    function GetJSONArray: TJsonArray;

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

    { exceptions  }
    [MVCPath('/exception1')]
    function GetMVCException: Integer;

    [MVCPath('/exception2')]
    function GetGeneralException: Integer;



    { using IMVCResponse and Response Methods}
    [MVCPath('/mvcresponse/message')]
    function GetMVCResponseSimple: IMVCResponse;
    [MVCPath('/mvcresponse/data')]
    function GetMVCResponseWithData: IMVCResponse;
    [MVCPath('/mvcresponse/data/message')]
    function GetMVCResponseWithDataAndMessage: IMVCResponse;
    [MVCPath('/mvcresponse/json')]
    function GetMVCResponseWithJSON: IMVCResponse;
    [MVCPath('/mvcresponse/list')]
    function GetMVCResponseWithObjectList: IMVCResponse;
    [MVCPath('/mvcresponse/dictionary')]
    function GetMVCResponseWithObjectDictionary: IMVCResponse;
    [MVCPath('/mvcresponse/message/builder/headers')]
    function GetMVCResponseSimpleBuilderWithHeaders: IMVCResponse;
    [MVCPath('/mvcresponse/message/builder/nobody')]
    function GetMVCResponseNoBody: IMVCResponse;
    [MVCPath('/mvcresponse/ok')]
    function GetOKResponse: IMVCResponse;
    [MVCPath('/mvcresponse/not_found')]
    function GetNotFound: IMVCResponse;
    [MVCPath('/mvcresponse/not_modified')]
    function GetNotModified: IMVCResponse;
    [MVCPath('/mvcresponse/accepted')]
    function GetAccepted: IMVCResponse;
    [MVCPath('/mvcresponse/generic')]
    function GetStatusResponse: IMVCResponse;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, System.DateUtils,
  MainDMU, FireDAC.Comp.Client, MVCFramework.FireDAC.Utils, System.IOUtils;

{ TMyController }

function TMyController.GetSeaPhoto(const ID: Integer): TStream;
var
  lBasePath: String;
begin
  lBasePath := TPath.Combine(TPath.Combine(AppPath, '..', '..','..'), '_', 'Image%.5d.jpg');
  lBasePath := Format(lBasePath, [ID]);
  if not TFile.Exists(lBasePath) then
  begin
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'File not found');
  end
  else
  begin
    ContentType := TMVCMediaType.IMAGE_PNG;
    Result := TFileStream.Create(lBasePath, fmOpenRead or fmShareDenyWrite);
  end;
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

function TMyController.GetMVCException: Integer;
begin
  raise EMVCException.Create(HTTP_STATUS.NotFound, 'Resource not found');
end;

function TMyController.GetMVCResponseNoBody: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Header('header1', 'Hello World')
    .Header('header2', 'foo bar')
    .Build;
end;

function TMyController.GetMVCResponseSimple: IMVCResponse;
begin
  Result := OKResponse('My Message');
end;

function TMyController.GetMVCResponseSimpleBuilderWithHeaders: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Header('header1', 'Hello World')
    .Header('header2', 'foo bar')
    .Body('My Message')
    .Build;
end;

function TMyController.GetMVCResponseWithData: IMVCResponse;
begin
  Result := OKResponse(TPerson.Create('Daniele','Teti', 99));
end;

function TMyController.GetMVCResponseWithDataAndMessage: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body('This is a message')  //<< Message
    .Body(TPerson.Create('Daniele','Teti', 99)) //<< Data
    .Body(ObjectDict().Add('person', TPerson.Create('Daniele','Teti', 99)))
    .Build;
end;

function TMyController.GetMVCResponseWithObjectDictionary: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(ObjectDict()
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
  )
  .Build;
end;

function TMyController.GetMVCResponseWithObjectList: IMVCResponse;
begin
  Result := OKResponse(TObjectList<TPerson>.Create([
              TPerson.Create('Daniele','Teti', 99),
              TPerson.Create('Peter','Parker', 25),
              TPerson.Create('Bruce','Banner', 45)
            ]));
end;

function TMyController.GetNotFound: IMVCResponse;
begin
  Result := NotFoundResponse;
end;

function TMyController.GetNotModified: IMVCResponse;
begin
  Result := NotModifiedResponse;
end;

function TMyController.GetOKResponse: IMVCResponse;
begin
  Result := OKResponse;
end;

function TMyController.GetOrTruthTable(const A, B: Boolean): Boolean;
begin
  Result := A or B;
end;

function TMyController.GetMVCResponseWithJSON: IMVCResponse;
begin
  Result := OKResponse(StrToJSONObject('{"name":"Daniele","surname":"Teti"}'));
end;

function TMyController.GetSingleObject: TPerson;
begin
  Result := TPerson.Create('Daniele', 'Teti', YearsBetween(Date, EncodeDateDay(1979, 1)));
end;

function TMyController.GetMultipleObjects: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;
  Result.Add(TPerson.Create('Daniele', 'Teti', YearsBetween(Date, EncodeDateDay(1979, 1))));
  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 10));
  Result.Add(TPerson.Create('Daniele', 'Teti', Result[0].Age + 20));
end;

function TMyController.GetJSONArray: TJsonArray;
begin
  Result := StrToJSONArray('[1,2,3, {"name":"Daniele","surname":"Teti"}]');
end;

function TMyController.GetJSONObject: TJSONObject;
begin
  Result := StrToJSONObject('{"name":"Daniele","surname":"Teti"}');
end;

function TMyController.GetSingleRecord: TPersonRec;
begin
  Result := TPersonRec.Create;
end;

function TMyController.GetStatusResponse: IMVCResponse;
begin
  Result := StatusResponse(HTTP_STATUS.InternalServerError, 'Hello There')
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


function TMyController.GetSum(const A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TMyController.GetSumAsFloat(const A, B: Extended): Extended;
begin
  Result := A + B;
end;

function TMyController.GetAccepted: IMVCResponse;
begin
  Result := AcceptedResponse('https://www.danieleteti.it');
end;

function TMyController.GetConcatAsString(const A, B: String): String;
begin
  Result :=  A + B;
end;

function TMyController.GetCustomerPhoto(const ID: Integer): TStream;
begin
  ContentType := TMVCMediaType.IMAGE_PNG;  // you can also use MVCProduces attribute
  Result := TFileStream.Create('..\..\..\_\customer.png', fmOpenRead or fmShareDenyWrite);
end;

function TMyController.GetGeneralException: Integer;
begin
  raise Exception.Create('This is a general exception');
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
