unit Services.ConnectionU;

interface

uses Services.InterfacesU;

type
  TConnectionService = class(TInterfacedObject, IConnectionService)
  protected
    function GetConnectionName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  MVCFramework.Logger;

{ TConnectionService }

constructor TConnectionService.Create;
begin
  inherited;
  LogI('Service ' + ClassName + ' created');
end;

destructor TConnectionService.Destroy;
begin
  LogI('Service ' + ClassName + ' destroyed');
  inherited;
end;

function TConnectionService.GetConnectionName: string;
begin
  Result := 'MyDemoConnection';
end;

end.
