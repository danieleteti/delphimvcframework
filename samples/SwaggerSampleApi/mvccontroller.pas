unit mvccontroller;

interface


uses
    MVCFramework
  , MVCFramework.Commons
  , MVCFramework.Logger
  , MVCFramework.JWT
  , Generics.Collections
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
  TMyMVCController = class(TMVCController)
    [MVCDoc('Returns a employee list.')]
    [MVCPath('/employees')]
    [MVCHTTPMethod([httpget])]
    [MVCResponseList(200, 'Successfully returns data', TEmployee)]
    procedure GetEmployeeList;

    [MVCDoc('Creates a employee.')]
    [MVCPath('/employees')]
    [MVCHTTPMethod([httppost])]
    [MVCResponse(201, 'Successfully creates data', TEmployee)]
    procedure CreateEmployee;

    [MVCDoc('Returns a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpget])]
    [MVCResponse(200, 'Successfully returns data', TEmployee)]
    procedure GetEmployee(id: Integer);

    [MVCDoc('Updates a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpput])]
    [MVCResponse(200, 'Successfully updates data', TEmployee)]
    procedure UpdateEmployee(id: Integer);

    [MVCDoc('Deletes a employee.')]
    [MVCPath('/employees/($id)')]
    [MVCHTTPMethod([httpdelete])]
    procedure DeleteEmployee(id: Integer);

  end;


implementation


uses
    Swag.Doc
  ;



procedure TMyMVCController.GetEmployeeList;
var
  employee : TObjectList<TEmployee>;
begin
  employee := TObjectList<TEmployee>.Create;

end;

procedure TMyMVCController.CreateEmployee;
var
  employee : TEmployee;
  paramEmployee : String;
begin
  employee := TEmployee.Create;
  paramEmployee := Context.Request.Params['employee'];

end;

procedure TMyMVCController.GetEmployee(id: Integer);
var
  employee : TEmployee;
begin
  employee := TEmployee.Create;

end;

procedure TMyMVCController.UpdateEmployee(id: Integer);
var
  employee : TEmployee;
  paramEmployee : String;
begin
  employee := TEmployee.Create;
  paramEmployee := Context.Request.Params['employee'];

end;

procedure TMyMVCController.DeleteEmployee(id: Integer);
begin

end;

end.
