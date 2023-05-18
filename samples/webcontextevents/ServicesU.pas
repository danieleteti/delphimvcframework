unit ServicesU;

interface

type
  ICalculator = interface
    ['{251FB86D-1A8B-4BBA-9487-B52AF8E18536}']
    function DoCalc(a,b: Integer): Integer;
  end;

  TMyService = class(TInterfacedObject, ICalculator)
  public
    function DoCalc(a,b: Integer): Integer;
    destructor Destroy; override;
  end;

implementation

{ TMyService }

destructor TMyService.Destroy;
begin

  inherited;
end;

function TMyService.DoCalc(a, b: Integer): Integer;
begin
  Result := a + b;
end;

end.
