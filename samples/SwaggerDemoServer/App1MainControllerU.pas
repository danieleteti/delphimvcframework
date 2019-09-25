unit App1MainControllerU;

interface

{$I dmvcframework.inc}

uses MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
  System.Json,
  Web.HTTPApp;

type

  TSubAddress = class(TObject)
    fsubid: Integer;
  public
    property SubId : Integer read fSubId write FSubId;
  end;


  TAddress = class(TObject)
  private
    fPostalCode: string;
    fCountry: string;
    fDescription: string;
    fCity: string;
    fRegion: string;
    fSubAddress: TSubAddress;
  public
    property Description: string read fDescription write fDescription;
    [MVCDoc('City for Address')]
    property City: string read fCity write fCity;
    property Region: string read fRegion write fRegion;
    property Country: string read fCountry write fCountry;
    [MVCPattern('\d\d\d\d')]
    property PostalCode: string read fPostalCode write fPostalCode;
    property SubAddress: TSubAddress read fSubAddress write fSubAddress;
  end;


  TEmployee = class(TObject)
  strict private
    fName: String;
    fHireDate: TDate;
    fPhone: String;
    fId: Int64;
    fSSN: String;
    fSalary: Double;
    fAddress: array [0..1] of TAddress;
    fAddressType: string;
  private
    function GetAddress(index: Integer): TAddress;
    procedure SetAddress(index: Integer; const Value: TAddress);
  published
    [JSONName('ID')]
    property Id: Int64 read fId write fId;
    property Name: String read fName write fName;
    property Phone: String read fPhone write fPhone;
    property HireDate: TDate read fHireDate write fHireDate;
    [MVCPattern('^\d{3}-\d{2}-\d{4}$')]
    [MVCDoc('Specifies the Employees Social Security Number')]
    property SSN : string read FSSN write FSSN;
    property Salary: Double read fSalary write fSalary;
    [MVCStringEnum('Home')]
    [MVCStringEnum('Business')]
    property AddressType : string read FAddressType write FAddressType;
  public
    property Address[index :Integer]: TAddress read GetAddress write SetAddress;
  end;


  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(200, 'success')]
    procedure HelloWorld;

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes('application/json')]
    procedure HelloWorldPost;

    [MVCPath('/hello2/($par1)/($par2)')]
    [MVCHTTPMethod([httpPOST,httpGET])]
    [MVCResponse(200, 'success')]
    [MVCResponse(401, 'unauthorized')]
    procedure HelloWorld2;

    [MVCPath('/Employee/($id)')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(200, 'success', TEmployee)]
    procedure GetEmployee;


    [MVCPath('/div/($par1)/($par2)')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/json')]
    [MVCResponse(200, 'success')]
    [MVCResponse(401, 'unauthorized')]
    procedure RaiseException([MVCDoc('First Parameter')] par1, [MVCDoc('Second Parameter')] par2: string);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.SystemJSONUtils;

{ TApp1MainController }

procedure TApp1MainController.GetEmployee;
begin

end;

procedure TApp1MainController.HelloWorld;
begin
  Render('Hello World called with GET');
  if Context.Request.ThereIsRequestBody then
    Log.Info('Body:' + Context.Request.Body, 'basicdemo');
end;

procedure TApp1MainController.HelloWorld2;
begin

end;

procedure TApp1MainController.HelloWorldPost;
var
  JSON: TJSONObject;
begin
  JSON := TSystemJSON.StringAsJSONObject(Context.Request.Body);
  try
    JSON.AddPair('modified', 'from server');
    Render(JSON, false);
  finally
    JSON.Free;
  end;
  Log.Info('Hello world called with POST', 'basicdemo');
end;

procedure TApp1MainController.Index;
begin
  Redirect('index.html');
end;

procedure TApp1MainController.RaiseException(par1, par2: string);
var
  R: Extended;
begin
  Log.Info('Parameter1=' + QuotedStr(par1), 'basicdemo');
  Log.Info('Parameter2=' + QuotedStr(par2), 'basicdemo');
  R := StrToInt(par1) / StrToInt(par2);
  Render(TJSONObject.Create(TJSONPair.Create('result', TJSONNumber.Create(R))));
end;

{ TEmployee }

function TEmployee.GetAddress(index: Integer): TAddress;
begin
  Result := fAddress[index];
end;

procedure TEmployee.SetAddress(index: Integer; const Value: TAddress);
begin
  fAddress[index] := Value;
end;

end.
