unit BusinessObjects;

interface

uses
  ObjectsMappers;

type
  TBaseBO = class
  private
    FID: Integer;
    procedure SetID(const Value: Integer);
  public
    procedure CheckInsert; virtual;
    procedure CheckUpdate; virtual;
    procedure CheckDelete; virtual;
  public
    property ID: Integer read FID write SetID;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  TArticle = class(TBaseBO)
  private
    FPrice: Currency;
    FCode: string;
    FDescription: string;
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetPrice(const Value: Currency);
  public
    procedure CheckDelete; override;
  public
    [MapperColumn('CODICE')]
    property Code: string read FCode write SetCode;
    [MapperColumn('DESCRIZIONE')]
    property Description: string read FDescription write SetDescription;
    [MapperColumn('PREZZO')]
    property Price: Currency read FPrice write SetPrice;
  end;

implementation

uses
  System.SysUtils;

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
  if Price > 0 then
    raise Exception.Create('Cannot delete an article with a price greater than 0 (yes, it is a silly check)');
end;

procedure TArticle.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TArticle.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TArticle.SetPrice(const Value: Currency);
begin
  FPrice := Value;
end;

end.
