unit mvccontrollerclient;

interface


uses
    IPPeerClient
  , REST.Client
  , REST.Authenticator.OAuth
  , REST.Types
  , Generics.Collections
  , MVCFramework
  , MVCFramework.Commons
  ;


(*
Title: Sample API
Description: Sample API Description
License: Apache License - Version 2.0, January 2004
*)

type
  TAddress = class
    [MVCDoc('The employee address description.')]
    description : string;

    [MVCDoc('The employee address city.')]
    city : string;

    [MVCDoc('The employee address region.')]
    region : string;

    [MVCDoc('The employee address country.')]
    country : string;

    [MVCDoc('The employee address postal code.')]
    postalCode : string;

  end;

  TEmployee = class
    [MVCDoc('The employee full name.')]
    [MVCMaxLength(80)]
    name : string;

    [MVCDoc('The employee ID Number.')]
    [MVCFormat('int64')]
    [MVCMinimum(0)]
    [MVCMaximum(99999999)]
    empId : integer;

    [MVCDoc('The employee phone number.')]
    phone : string;

    [MVCDoc('The employee hire date.')]
    [MVCFormat('yyyy-MM-dd')]
    hireDate : string;

    [MVCDoc('The employee gross salary.')]
    salary : Double;

    [MVCDoc('The employee full address.')]
    address : TAddress;

  end;

  [MVCPath('/api')]
  TMyMVCControllerClient = class(TObject)
    RESTClient : TRESTClient;

    RESTRequest : TRESTRequest;

    RESTResponse : TRESTResponse;

    [MVCDoc('Returns a employee list.')]
    [MVCPath('/employees')]
    [MVCHTTPMethod([httpget])]
    [MVCResponseList(200, 'Successfully returns data', TEmployee)]
    procedure GetEmployeeList;

    [MVCDoc('Creates a employee.')]
    [MVCPath('/employees')]
    [MVCHTTPMethod([httppost])]
    [MVCResponse(201, 'Successfully creates data', TEmployee)]
    procedure CreateEmployee(paramEmployee: TEmployee);

    [MVCDoc('Returns a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpget])]
    [MVCResponse(200, 'Successfully returns data', TEmployee)]
    procedure GetEmployee(paramid: Integer);

    [MVCDoc('Updates a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpput])]
    [MVCResponse(200, 'Successfully updates data', TEmployee)]
    procedure UpdateEmployee(paramid: Integer; paramEmployee: TEmployee);

    [MVCDoc('Deletes a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpdelete])]
    procedure DeleteEmployee;

  end;


implementation


uses
    Swag.Doc
  ;



procedure TMyMVCControllerClient.GetEmployeeList;
var
  employee : TObjectList<TEmployee>;
begin
  employee := TObjectList<TEmployee>.Create;

end;

procedure TMyMVCControllerClient.CreateEmployee(paramEmployee: TEmployee);
var
  employee : TEmployee;
begin
  employee := TEmployee.Create;

end;

procedure TMyMVCControllerClient.GetEmployee(paramid: Integer);
var
  employee : TEmployee;
begin
  employee := TEmployee.Create;

end;

procedure TMyMVCControllerClient.UpdateEmployee(paramid: Integer; paramEmployee: TEmployee);
var
  employee : TEmployee;
begin
  employee := TEmployee.Create;

end;

procedure TMyMVCControllerClient.DeleteEmployee;
begin

end;

end.
