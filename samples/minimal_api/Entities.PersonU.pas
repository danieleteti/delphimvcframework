unit Entities.PersonU;

interface

uses
  System.Generics.Collections,
  MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncCamelCase)]
  TPerson = class
  private
    fID: Integer;
    fFirstName: string;
    fLastName: string;
    fAge: Integer;
  public
    property ID: Integer read fID write fID;
    property FirstName: string read fFirstName write fFirstName;
    property LastName: string read fLastName write fLastName;
    property Age: Integer read fAge write fAge;
  end;

  TPeopleList = TObjectList<TPerson>;

  // Group data for the /v1 and /v2 prefix groups in the demo.
  // Carried by TMVCRouteGroup<TApiVersion>.
  [MVCNameCase(ncCamelCase)]
  TApiVersion = class
  private
    fNumber: Integer;
    fDeprecated: Boolean;
    fSunset: string;
  public
    constructor Create(const ANumber: Integer; const ADeprecated: Boolean;
      const ASunset: string = ''); reintroduce;
    property Number: Integer read fNumber;
    property Deprecated: Boolean read fDeprecated;
    property Sunset: string read fSunset;
  end;

implementation

constructor TApiVersion.Create(const ANumber: Integer;
  const ADeprecated: Boolean; const ASunset: string);
begin
  inherited Create;
  fNumber := ANumber;
  fDeprecated := ADeprecated;
  fSunset := ASunset;
end;

end.
