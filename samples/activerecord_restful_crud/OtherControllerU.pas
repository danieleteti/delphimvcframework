unit OtherControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  TOtherController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function GetSomethings: String;
    [MVCPath('/else')]
    [MVCHTTPMethods([httpGET])]
    function GetSomethingElse: String;
  end;

implementation

{ TCustomController }

function TOtherController.GetSomethingElse: String;
begin
  Result := 'Hello There, it''s "GetSomethingElse" here';
end;

function TOtherController.GetSomethings: String;
begin
  Result := 'Hello There, it''s "GetSomethings" here';
end;

end.
