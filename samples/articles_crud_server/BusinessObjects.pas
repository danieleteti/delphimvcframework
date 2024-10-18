unit BusinessObjects;

interface

uses
  MVCFramework.Serializer.Commons, MVCFramework.Nullables,
  MVCFramework.ActiveRecord;

type
  TBaseBO = class(TMVCActiveRecord)
  private
    [MVCTableField('ID', [foPrimaryKey])]
    FID: Integer;
    procedure SetID(const Value: Integer);
  public
    property ID: Integer read FID write SetID;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('ARTICOLI')]
  [MVCNamedSQLQuery(
    'search_by_text',
    'SELECT * FROM ARTICOLI WHERE DESCRIZIONE CONTAINING ? ORDER BY ID')
    ]
  TArticle = class(TBaseBO)
  private
    [MVCTableField('CODICE')]
    FCode: string;
    [MVCTableField('DESCRIZIONE')]
    FDescription: String;
    [MVCTableField('PREZZO')]
    FPrice: Currency;
    [MVCTableField('UPDATED_AT')]
    FUpdatedAt: TDateTime;
    [MVCTableField('CREATED_AT')]
    FCreatedAt: TDateTime;
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: String);
    procedure SetPrice(const Value: Currency);
    procedure SetCreatedAt(const Value: TDateTime);
    procedure SetUpdatedAt(const Value: TDateTime);
  protected
    procedure OnBeforeInsertOrUpdate; override;
    procedure OnBeforeUpdate; override;
    procedure OnBeforeDelete; override;
  public
    property Code: string read FCode write SetCode;
    property Description: String read FDescription write SetDescription;
    property Price: Currency read FPrice write SetPrice;
    property CreatedAt: TDateTime read FCreatedAt write SetCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write SetUpdatedAt;
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;

{ TBaseBO }

procedure TBaseBO.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TArticle.OnBeforeDelete;
begin
  inherited;
  if Price <= 5 then
    raise Exception.Create('Cannot delete an article with a price below 5 euros (yes, it is a silly check)');
end;

procedure TArticle.OnBeforeInsertOrUpdate;
begin
  inherited;
  if not TRegEx.IsMatch(Code, '^C[0-9]{2,4}') then
    raise Exception.Create('Article code must be in the format "CXX or CXXX or CXXXX"');
end;

procedure TArticle.OnBeforeUpdate;
begin
  inherited;
  if Price <= 2 then
    raise Exception.Create('We cannot sell so low cost pizzas!');
end;

procedure TArticle.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TArticle.SetCreatedAt(const Value: TDateTime);
begin
  FCreatedAt := Value;
end;

procedure TArticle.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TArticle.SetPrice(const Value: Currency);
begin
  FPrice := Value;
end;

procedure TArticle.SetUpdatedAt(const Value: TDateTime);
begin
  FUpdatedAt := Value;
end;

end.
