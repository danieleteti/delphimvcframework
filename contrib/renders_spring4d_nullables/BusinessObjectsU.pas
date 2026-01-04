unit BusinessObjectsU;

interface

uses
  System.Classes,
  Spring,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type

  [MVCNameCase(ncLowerCase)]
  TPerson = class
  private
    FID: Int64;
    FIDManager: TNullableInteger;
    FSalary: TNullableCurrency;
    FLastName: string;
    FMiddleName: TNullableString;
    FFirstName: string;
    FPhoto: TMemoryStream;
    FNotes: TStringStream;
    procedure SetID(const Value: Int64);
    procedure SetIDManager(const Value: TNullableInteger);
    procedure SetSalary(const Value: TNullableCurrency);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetMiddleName(const Value: TNullableString);
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Int64 read FID write SetID;
    property IDManager: TNullableInteger read FIDManager write SetIDManager;
    property Salary: TNullableCurrency read FSalary write SetSalary;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property MiddleName: TNullableString read FMiddleName write SetMiddleName;
    property Photo: TMemoryStream read FPhoto;
    [MVCSerializeAsString]
    property Notes: TStringStream read FNotes;
  end;

  [MVCNameCase(ncLowerCase)]
  TParent = class
  private
    FPerson: TPerson;
    FFullName: TNullableString;
    procedure SetPerson(const Value: TPerson);
    procedure SetFullName(const Value: TNullableString);
  public
    constructor Create;
    destructor Destroy; override;
    property FullName: TNullableString read FFullName write SetFullName;
    property Person: TPerson read FPerson write SetPerson;
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.NullableTypes;

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  FPhoto := TMemoryStream.Create;
  FNotes := TStringStream.Create;
  FNotes.WriteString('This is a note');
  FPhoto.CopyFrom(FNotes, 0);
end;

destructor TPerson.Destroy;
begin
  FPhoto.Free;
  FNotes.Free;
  inherited;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetID(const Value: Int64);
begin
  FID := Value;
end;

procedure TPerson.SetIDManager(const Value: TNullableInteger);
begin
  FIDManager := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPerson.SetMiddleName(const Value: TNullableString);
begin
  FMiddleName := Value;
end;

procedure TPerson.SetSalary(const Value: TNullableCurrency);
begin
  FSalary := Value;
end;

{ TParent }

constructor TParent.Create;
begin
  inherited;
  FPerson := TPerson.Create;
end;

destructor TParent.Destroy;
begin
  FPerson.Free;
  inherited;
end;

procedure TParent.SetFullName(const Value: TNullableString);
begin
  FFullName := Value;
end;

procedure TParent.SetPerson(const Value: TPerson);
begin
  FPerson := Value;
end;

end.
