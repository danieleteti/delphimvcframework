unit uServices;
// Services module which provides binding between the data model and the persistence layer
// for sample app this will persist data into a json file.
// In a proper application this would be making calls to a persistent layer like a database etc

interface

uses
  System.Generics.Collections,
  uData.Model,
  System.SysUtils,
  JsonDataObjects;

type
  TServiceBase = class abstract
  public
  end;

  TGenreService = class(TServiceBase)
  public const
    Names: array [0 .. 6] of string = ('Horror', 'Comedy', 'Action', 'Romance', 'Sci-Fi', 'Drama', 'Documentary');
  public
    function GetGenresAsList(const Selected: Integer = 0): TObjectList<TGenre>;
  end;

  TMovieService = class(TServiceBase)
  private
    function GetFileName: string;
    function LoadFromFile: string;
    procedure SaveToFile(Data: string);
  public
    function GetByID(const MovieID: Integer): TMovie;
    function ListAll: TObjectList<TMovie>;
    function ListBySearchTerm(SearchTerm: string): TObjectList<TMovie>;
    function UpdateMovie(const Movie: TMovie; InsertMode: Boolean): Boolean;
    function DeleteMovie(const MovieID: Integer): Boolean;
    function GetNextID: Integer;
    function ListAll2: TObjectList<TMovie>;
  end;

implementation

uses
  MVCFramework.Serializer.Intf, MVCFramework.Serializer.JsonDataObjects,
  System.Classes, System.IOUtils, MVCFramework.Serializer.Defaults;

{ TGenreService }

function TGenreService.GetGenresAsList(const Selected: Integer = 0): TObjectList<TGenre>;
var
  Genre: TGenre;
begin
  Result := TObjectList<TGenre>.Create;
  for var I := Low(Names) to High(Names) do
  begin
    Genre := TGenre.Create(I + 1, Names[I]);
    if Selected = I + 1 then
      Genre.Selected := true;
    Result.Add(Genre);
  end;
end;

{ TMovieService }

function TMovieService.GetNextID: Integer;
var
  Data: TObjectList<TMovie>;
begin
  Result := 0;
  Data := ListAll;
  try
    for var Movie in Data do
      if Movie.MovieID > Result then
        Result := Movie.MovieID;
    Inc(Result);
  finally
    Data.Free;
  end;
end;

function TMovieService.ListAll2: TObjectList<TMovie>;
var
  Movie: TMovie;
begin
  Result := TObjectList<TMovie>.Create;
  Movie := TMovie.Create;
  Movie.MovieID := 1;
  Movie.MovieName := 'gone';
  Result.Add(Movie);
end;

function TMovieService.ListAll: TObjectList<TMovie>;
var
  Data: string;
  Json: TJsonArray;
begin
  Data := LoadFromFile;
  Json := TJsonBaseObject.Parse(Data) as TJsonArray;
  try
    Result := TJSONUtils.JSONArrayToListOf<TMovie>(Json);
    for var I := 0 to Result.Count - 1 do
    begin
      Result[I].GenreName := TGenreService.Names[Result[I].GenreID - 1];
    end;
  finally
    Json.Free;
  end;
end;

function TMovieService.ListBySearchTerm(SearchTerm: string): TObjectList<TMovie>;
begin
  Result := ListAll;
  if not SearchTerm.IsEmpty then
    for var I := Result.Count - 1 downto 0 do
      if not Result[I].MovieName.ToUpper.Contains(SearchTerm.ToUpper) then
        Result.Delete(I);;
end;

function TMovieService.DeleteMovie(const MovieID: Integer): Boolean;
var
  Data: TObjectList<TMovie>;
begin
  Result := False;
  Data := ListAll;
  try
    for var Movie in Data do
    begin
      if Movie.MovieID = MovieID then
      begin
        Movie.CheckDelete;
        Data.Remove(Movie);
        Result := true;
        break;
      end;
    end;
    if Result then
      SaveToFile(GetDefaultSerializer.SerializeCollection(Data));
  finally
    Data.Free;
  end;
end;

function TMovieService.GetByID(const MovieID: Integer): TMovie;
var
  Data: TObjectList<TMovie>;
begin
  Result := nil;
  Data := ListAll;
  try
    for var Movie in Data do
    begin
      if Movie.MovieID = MovieID then
      begin
        Result := TMovie.Create;
        Result.Clone(Movie);
      end;
    end;
  finally
    Data.Free;
  end;
end;

function TMovieService.UpdateMovie(const Movie: TMovie; InsertMode: Boolean): Boolean;
var
  Data: TObjectList<TMovie>;
  Found: Boolean;
  Clone: TMovie;
begin
  Found := False;
  if InsertMode then
    Movie.CheckInsert
  else
    Movie.CheckUpdate;
  Data := ListAll;
  try
    Clone := TMovie.Create;
    Clone.Clone(Movie);
    for var I := 0 to Data.Count - 1 do
    begin
      if Data[I].MovieID = Movie.MovieID then
      begin
        Data[I] := Clone;
        Found := true;
        break;
      end;
    end;
    if not Found then
      Data.Add(Clone);
    SaveToFile(GetDefaultSerializer.SerializeCollection(Data));
    Result := true;
  finally
    Data.Free;
  end;
end;

function TMovieService.GetFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'data\';
  ForceDirectories(Result);
  Result := Result + 'data.json';
end;

function TMovieService.LoadFromFile: string;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  if not FileExists(GetFileName) then
    Exit('[]');
  Stream := TFileStream.Create(GetFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, Stream.Size);
    Stream.Read(Bytes, Stream.Size);
    Result := TEncoding.UTF8.GetString(Bytes);
    if Result = '' then
      Result := '[]';
  finally
    Stream.Free;
  end;
end;

procedure TMovieService.SaveToFile(Data: string);
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(Data);
  Stream := TFileStream.Create(GetFileName, fmCreate OR fmOpenWrite or fmShareDenyNone);
  try
    Stream.Write(Bytes, 0, Length(Bytes));
  finally
    Stream.Free;
  end;
end;

end.
