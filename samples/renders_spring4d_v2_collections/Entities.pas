unit Entities;

interface

uses
  MVCFramework.Serializer.Commons,
  Spring.Collections;

type
  TPhone = class;

  [MVCNameCase(ncSnakeCase)]
  TCustomer = class
  private
    FId: Integer;
    FistName: string;
    FLastName: string;
    FPhones: IList<TPhone>;
  public
    constructor Create;
    property Id: Integer read FId write FId;
    property FirstName: string read FistName write FistName;
    property LastName: string read FLastName write FLastName;
    property Phones: IList<TPhone> read FPhones write FPhones;
  end;

  [MVCNameCase(ncSnakeCase)]
  TPhone = class
  private
    FId: Integer;
    FDescription: string;
    FNumber: string;
  public
    constructor Create(const AId: Integer; const ADescription, ANumber: string);
    property Id: Integer read FId write FId;
    property Description: string read FDescription write FDescription;
    property Number: string read FNumber write FNumber;
  end;

implementation

{ TCustomer }

constructor TCustomer.Create;
begin
  inherited Create;
  FPhones := TCollections.CreateObjectList<TPhone>;
end;

{ TPhone }

constructor TPhone.Create(const AId: Integer; const ADescription, ANumber: string);
begin
  inherited Create;
  FId := AId;
  FDescription := ADescription;
  FNumber := ANumber;
end;

end.
