unit Services.PeopleU;

interface

uses
  System.Generics.Collections, Entities, Services.InterfacesU, MVCFramework.Logger;

type
  TPeopleService = class(TInterfacedObject, IPeopleService)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetAll: TObjectList<TPerson>;
  end;

implementation

{ TPeopleService }

constructor TPeopleService.Create;
begin
  inherited;
  LogI('Service ' + ClassName + ' created');
end;

destructor TPeopleService.Destroy;
begin
  LogI('Service ' + ClassName + ' destroyed');
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
