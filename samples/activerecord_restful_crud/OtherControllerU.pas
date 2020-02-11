unit OtherControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  TOtherController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    procedure GetSomethings;
    [MVCPath('/else')]
    [MVCHTTPMethods([httpGET])]
    procedure GetSomethingElse;
  end;

implementation

{ TCustomController }

procedure TOtherController.GetSomethingElse;
begin
  Render('Hello There, it''s "GetSomethingElse" here');
end;

procedure TOtherController.GetSomethings;
begin
  Render('Hello There, it''s "GetSomethings" here');
end;

end.
