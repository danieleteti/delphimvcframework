unit uData.Model;

interface

uses
  MVCFramework.Serializer.Commons, MVCFramework.Nullables;

type
  TBaseBO = class
  protected
    procedure Clone(Source: TBaseBO); virtual; abstract;
  public
    constructor Create; virtual;
    procedure CheckInsert; virtual;
    procedure CheckUpdate; virtual;
    procedure CheckDelete; virtual;
  end;

  [MVCNameCase(ncLowerCase)]
  TGenre = class(TBaseBO)
  private
    FGenreID: Integer;
    FGenre: string;
    FSelected: Boolean;
  protected
    procedure Clone(Source: TBaseBO); override;
  public
    constructor Create(AID: Integer; AName: string); reintroduce;
    property GenreID: Integer read FGenreID write FGenreID;
    property Genre: string read FGenre write FGenre;
    // this is a dummy property - it gets set when rendering a list with an existing genre
    property Selected: Boolean read FSelected write FSelected;
  end;

  [MVCNameCase(ncAsIs)]
  TMovie = class(TBaseBO)
  private
    FMovieID: Integer;
    FGenreID: Integer;
    FCurrent: Boolean;
    FMovieName: string;
    FGenreName: string;
    FRating: Integer;
    FReleaseDate: TDate;
    procedure SetGenreID(const Value: Integer);
    procedure SetCurrent(const Value: Boolean);
    procedure SetReleaseDate(const Value: TDate);
  public
    constructor CreateNew(ACurrent: Boolean);
    procedure Clone(Source: TBaseBO); override;
    procedure CheckInsert; override;
    procedure CheckUpdate; override;
    procedure CheckDelete; override;
    property MovieID: Integer read FMovieID write FMovieID;
    property MovieName: string read FMovieName write FMovieName;
    property Rating: Integer read FRating write FRating;
    property GenreID: Integer read FGenreID write SetGenreID;
    [MVCDoNotDeSerialize]
    property GenreName: string read FGenreName write FGenreName;
    property Current: Boolean read FCurrent write SetCurrent;
    property ReleaseDate: TDate read FReleaseDate write SetReleaseDate;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils, System.DateUtils;

{ TBaseBO }

procedure TBaseBO.CheckDelete;
begin
  // override in descendant
end;

procedure TBaseBO.CheckInsert;
begin
  // override in descendant
end;

procedure TBaseBO.CheckUpdate;
begin
  // override in descendant
end;

constructor TBaseBO.Create;
begin
  inherited Create;
end;

{ TGenre }

procedure TGenre.Clone(Source: TBaseBO);
begin
  if Source is TGenre then
  begin
    FGenreID := TGenre(Source).FGenreID;
    FGenre := TGenre(Source).FGenre;
    FSelected := TGenre(Source).FSelected;
  end;
end;

constructor TGenre.Create(AID: Integer; AName: string);
begin
  inherited Create;
  FGenreID := AID;
  FGenre := AName;
end;

{ TMovie }

procedure TMovie.CheckDelete;
begin
  inherited;
  if Current then
    raise Exception.Create('Cannot delete a current movie');
end;

procedure TMovie.CheckInsert;
begin
  inherited;
  if FMovieID <= 0 then
    raise Exception.Create('Must supply Movie ID');
  if (FRating < 0) or (FRating > 10) then
    raise Exception.Create('Rating must be between 0 and 10');
  if FReleaseDate < EncodeDate(1900, 1, 1) then
    raise Exception.Create('Movies cannot be older than 1st Jan 1900');
end;

procedure TMovie.CheckUpdate;
begin
  inherited;
  CheckInsert;
end;

procedure TMovie.Clone(Source: TBaseBO);
begin
  if Source is TMovie then
  begin
    FMovieID := TMovie(Source).FMovieID;
    FGenreID := TMovie(Source).FGenreID;
    FGenreName := TMovie(Source).FGenreName;
    FCurrent := TMovie(Source).FCurrent;
    FMovieName := TMovie(Source).FMovieName;
    FRating := TMovie(Source).FRating;
    FReleaseDate := TMovie(Source).FReleaseDate;
  end;
end;

constructor TMovie.CreateNew(ACurrent: Boolean);
begin
  inherited Create;
  FCurrent := ACurrent;
  ReleaseDate := Now;
end;

procedure TMovie.SetGenreID(const Value: Integer);
begin
  FGenreID := Value;
end;

procedure TMovie.SetReleaseDate(const Value: TDate);
begin
  FReleaseDate := Value;
end;

procedure TMovie.SetCurrent(const Value: Boolean);
begin
  FCurrent := Value;
end;

end.
