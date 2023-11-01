unit uMovie.Controller;

interface

uses
  MVCFramework, MVCFramework.Commons, uBase.Controller, JsonDataObjects;

type

  [MVCPath('/movie')]
  TMovieController = class(TBaseController)
  private
    FInsertMode: Boolean;
    FIncludeBlankRow: Boolean;
  protected
    procedure RenderForm(const AViewNames: TArray<string>);
  public
    [MVCDoc('Trigger a client side error')]
    [MVCPath('/error/400'), MVCHTTPMethod([httpGET])]
    [MVCConsumes('application/json')]
    procedure TriggerError;

    [MVCDoc('Ask server to swap something on the page')]
    [MVCPath('/swap'), MVCHTTPMethod([httpPATCH])]
    [MVCConsumes('application/json')]
    [MVCProduces('text/html')]
    procedure TriggerSwap;

    [MVCDoc('Get the list of Movies')]
    [MVCPath('/page'), MVCHTTPMethod([httpGET])]
    [MVCProduces('text/html')]
    procedure GetMoviesPage;

    [MVCDoc('Search for movies')]
    [MVCPath('/search'), MVCHTTPMethod([httpPOST])]
    [MVCConsumes('application/json')]
    [MVCProduces('text/html')]
    procedure SearchMovies;

    [MVCDoc('Get the page to edit an individual Movie')]
    [MVCPath('/($MovieID)/edit'), MVCHTTPMethod([httpGET])]
    [MVCProduces('text/html')]
    procedure GetEditPanel(MovieID: Integer);

    [MVCDoc('Render a dialog for an insert')]
    [MVCPath('/insert'), MVCHTTPMethod([httpGET])]
    [MVCProduces('text/html')]
    procedure GetInsertPanel;

    [MVCDoc('Render a specified Movie to the grid')]
    [MVCPath('/($MovieID)'), MVCHTTPMethod([httpGET])]
    [MVCProduces('text/html')]
    procedure GetMovie(MovieID: Integer);

    [MVCDoc('Update a specified Movie')]
    [MVCPath('/($MovieID)'), MVCHTTPMethod([httpPUT])]
    [MVCProduces('text/html')]
    procedure UpdateMovie(MovieID: Integer);

    [MVCDoc('Create a specified Movie')]
    [MVCPath(''), MVCHTTPMethod([httpPOST])]
    [MVCProduces('text/html')]
    procedure CreateMovie;

    [MVCDoc('Delete a specified Movie')]
    [MVCPath('/($MovieID)'), MVCHTTPMethod([httpDELETE])]
    [MVCConsumes('application/json')]
    [MVCProduces('text/html')]
    procedure DeleteMovie(MovieID: Integer);
  end;

implementation

uses
  MVCFramework.Utils,
  System.SysUtils,
  MVCFramework.Serializer.JsonDataObjects,
  uServices,
  System.Generics.Collections,
  uData.Model,
  MVCFramework.Serializer.Defaults,
  MVCFramework.HTMX;

{ TMovieController }

procedure TMovieController.RenderForm(const AViewNames: TArray<string>);
begin
  PageData.b['InsertMode'] := FInsertMode;
  PageData.b['IncludeBlankRow'] := FIncludeBlankRow;
  LoadView(AViewNames);
  RenderResponseStream;
end;

/// // HTML rendering
procedure TMovieController.GetMoviesPage;
// render the Movies page
var
  Movies: TObjectList<TMovie>;
begin
  Movies := GetMovieService.ListAll;
  try
    ViewData['Movies'] := Movies;
    if Context.Request.IsHTMX then
    begin
      PageData.S['Explanation'] := 'Loaded via a seamless ajax call';
      // rendering with htmx, pudsh the URL into the browser bar so a page refresh will not go back to the index page
      Context.Response.HXSetPushUrl('/movie/page');
      // Context.Response.SetReswap(soInnerHTML,  ssScroll, sstBottom, '#theFooter');
      RenderForm(['Movie']);
    end
    else
    begin
      // just a normal form render
      PageData.S['Explanation'] :=
        'Loaded via a full page reload, watch for the flicker in the title bar and see the calls in the browser Dev Tools console (network tab)';
      RenderForm(['Header', 'Movie', 'Footer']);
    end;
  finally
    Movies.Free;
  end;
end;

procedure TMovieController.SearchMovies;
// Search the Movies page
var
  Params: TJsonObject;
  Movies: TObjectList<TMovie>;
begin
  Params := TJsonBaseObject.Parse(Context.Request.Body) as TJsonObject;
  Movies := GetMovieService.ListBySearchTerm(Params.S['search']);
  try
    ViewData['Movies'] := Movies;
    begin
      RenderForm(['MovieDataRow']);
    end;
  finally
    Movies.Free;
    Params.Free;
  end;
end;

procedure TMovieController.TriggerError;
// propogate an error from the server back to the web page
begin
  raise Exception.Create('Exception for client side');
end;

procedure TMovieController.TriggerSwap;
begin
  // make a server side delay so you can see the htmx indicator svg
  sleep(500);
  // now tell the web page to delay doing the content swap for another 1 second
  Context.Response.HXSetReswap(soInnerHTML, 1000);
  Render(Format('... Button was clicked at %s', [FormatDateTime('hh:mm:ss', Now)]));
end;

procedure TMovieController.GetEditPanel(MovieID: Integer);
// return the editor panel
var
  Genres: TObjectList<TGenre>;
  Movie: TMovie;
begin
  Movie := GetMovieService.GetByID(MovieID);
  Genres := GetGenreService.GetGenresAsList(Movie.GenreID);
  try
    ViewData['Movies'] := Movie;
    ViewData['Genres'] := Genres;
    Context.Response.HXTriggerClientEvent('setFocus', '.focus', etSwapped);
    RenderForm(['MovieDataEdit']);
  finally
    Movie.Free;
    Genres.Free;
  end;
end;

procedure TMovieController.GetInsertPanel;
// get the movie insert panel
var
  Movie: TMovie;
  Genres: TObjectList<TGenre>;
begin
  Movie := TMovie.CreateNew(true);
  Genres := GetGenreService.GetGenresAsList(Movie.GenreID);
  try
    Movie.MovieID := GetMovieService.GetNextID;
    FInsertMode := true;
    ViewData['Genres'] := Genres;
    ViewData['Movies'] := Movie;
    // make sure the insert panel is visible - this shows how to triiger evenst in the browser from the server
    Context.Response.HXTriggerClientEvent('setFocus', '.focus', etSwapped);
    RenderForm(['MovieDataEdit']);
  finally
    Genres.Free;
    Movie.Free;
  end;
end;

procedure TMovieController.GetMovie(MovieID: Integer);
// render out a specified movie
var
  Movie: TMovie;
begin
  Movie := GetMovieService.GetByID(MovieID);
  try
    if Movie <> nil then
      ViewData['Movies'] := Movie
    else
      FIncludeBlankRow := true;
    RenderForm(['MovieDataRow']);
  finally
    if Movie <> nil then
      Movie.Free;
  end;
end;

procedure TMovieController.CreateMovie;
// create a new Movie, and return it
var
  MovieID: Integer;
  Movie: TMovie;
begin
  Movie := Context.Request.BodyAs<TMovie>;
  try
    if not GetMovieService.UpdateMovie(Movie, true) then
      raise Exception.Create('Could not create Movie');
    MovieID := Movie.MovieID;
    // send the object back to the script on the page
    Context.Response.HXTriggerClientEvent('myEventObject', Movie);
  finally
    Movie.Free;
  end;
  FIncludeBlankRow := true;
  GetMovie(MovieID);
end;

procedure TMovieController.UpdateMovie(MovieID: Integer);
// handle the update, send back the modified record
var
  Movie: TMovie;
begin
  Movie := Context.Request.BodyAs<TMovie>;
  try
    if not GetMovieService.UpdateMovie(Movie, false) then
      raise Exception.Create('Could not update Movie');
  finally
    Movie.Free;
  end;
  Context.Response.HXTriggerClientEvent('savedEvent', 'Movie Saved OK');

  GetMovie(MovieID);
end;

procedure TMovieController.DeleteMovie(MovieID: Integer);
// delete the movie, render nothing back so <tr> gets remove from the web page
begin
  if not GetMovieService.DeleteMovie(MovieID) then
    raise Exception.Create('Could not delete Movie');
  Context.Response.HXTriggerClientEvent('savedEvent', 'Movie Deleted OK');

  RenderForm([]);
end;

end.
