unit ServicesInterfaceU;

interface

type
  IUsersService = interface
    ['{D54AF728-7688-40DE-B10C-E6D63949531E}']
    function GetUserNameByID(const ID: Integer): String;
  end;

  ICustomersService = interface
    ['{DC94C34E-13A2-4406-8961-6A407B792DD3}']
    function GetCustomerNameByID(const ID: Integer): String;
  end;

  ICommonService = interface
    ['{EAA26199-4142-4698-9C17-5D241D9984AA}']
    function GetID: String;
  end;

implementation

end.
