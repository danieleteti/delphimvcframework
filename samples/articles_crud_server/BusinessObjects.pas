unit BusinessObjects;

interface

uses
  MVCFramework.Serializer.Commons, MVCFramework.Nullables;

type
  TBaseBO = class
  private
    FID: Integer;
    procedure SetID(const Value: Integer);
  public
    procedure CheckInsert; virtual;
    procedure CheckUpdate; virtual;
    procedure CheckDelete; virtual;
    property ID: Integer read FID write SetID;
  end;

  [MVCNameCase(ncLowerCase)]
  TArticle = class(TBaseBO)
  private
    FPrice: Currency;
    FCode: string;
    FDescription: String;
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: String);
    procedure SetPrice(const Value: Currency);
  public
    procedure CheckInsert; override;
    procedure CheckUpdate; override;
    procedure CheckDelete; override;
    [MVCColumn('CODICE')]
    property Code: string read FCode write SetCode;
    [MVCColumn('DESCRIZIONE')]
    property Description: String read FDescription write SetDescription;
    [MVCColumn('PREZZO')]
    property Price: Currency read FPrice write SetPrice;
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;

{ TBaseBO }

procedure TBaseBO.CheckDelete;
begin

end;

procedure TBaseBO.CheckInsert;
begin

end;

procedure TBaseBO.CheckUpdate;
begin

end;

procedure TBaseBO.SetID(const Value: Integer);
begin
  FID := Value;
end;

{ TArticolo }

procedure TArticle.CheckDelete;
begin
  inherited;
  if Price <= 5 then
    raise Exception.Create('Cannot delete an article with a price below 5 euros (yes, it is a silly check)');
end;

procedure TArticle.CheckInsert;
begin
  inherited;
  if not TRegEx.IsMatch(Code, '^C[0-9]{2,4}') then
    raise Exception.Create('Article code must be in the format "CXX or CXXX or CXXXX"');
end;

procedure TArticle.CheckUpdate;
begin
  inherited;
  CheckInsert;
  if Price <= 2 then
    raise Exception.Create('We cannot sell so low cost pizzas!');
end;

procedure TArticle.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TArticle.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TArticle.SetPrice(const Value: Currency);
begin
  FPrice := Value;
end;

end.
