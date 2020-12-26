unit MyControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController) 
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/reversedstrings/($Value)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetReversedString(const Value: String);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController.Index;
begin
  //use Context property to access to the HTTP request and response 
  Render('Hello DelphiMVCFramework World');
end;

procedure TMyController.GetReversedString(const Value: String);
begin
  Render(System.StrUtils.ReverseString(Value.Trim));
end;




end.
