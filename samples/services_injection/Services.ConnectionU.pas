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
  MVCFramework.Logger, System.SysUtils;

{ TConnectionService }

constructor TConnectionService.Create;
begin
  inherited;
  LogI('Service ' + ClassName + ' created [' + IntToHex(NativeUInt(Pointer(Self))) + ']');
end;

destructor TConnectionService.Destroy;
begin
  LogI('Service ' + ClassName + ' destroyed [' + IntToHex(NativeUInt(Pointer(Self))) + ']');
  inherited;
end;

function TConnectionService.GetConnectionName: string;
begin
  Result := 'MyDemoConnection';
end;

end.
