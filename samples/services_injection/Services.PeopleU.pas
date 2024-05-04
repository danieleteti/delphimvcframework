unit Services.PeopleU;

interface

uses
  System.Generics.Collections, Entities, Services.InterfacesU, MVCFramework.Logger,
  MVCFramework, MVCFramework.Container;

type
  TPeopleService = class(TInterfacedObject, IPeopleService)
  private
    fConnService: IConnectionService;
  public
    [MVCInject]
    constructor Create(ConnectionService: IConnectionService); virtual;
    destructor Destroy; override;
    function GetAll: TObjectList<TPerson>;
  end;

implementation

uses
  System.SysUtils {IntToHex};

{ TPeopleService }

constructor TPeopleService.Create(ConnectionService: IConnectionService);
begin
  inherited Create;
  fConnService := ConnectionService;
  LogI('Service ' + ClassName + ' created [' + IntToHex(NativeUInt(Pointer(Self))) + ']');
end;

destructor TPeopleService.Destroy;
begin
  LogI('Service ' + ClassName + ' destroyed [' + IntToHex(NativeUInt(Pointer(Self))) + ']');
  inherited;
end;

function TPeopleService.GetAll: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;
  Result.AddRange([
    TPerson.Create,
    TPerson.Create,
    TPerson.Create,
    TPerson.Create
  ]);
end;

end.
