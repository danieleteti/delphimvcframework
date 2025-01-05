unit uBase.Controller;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, JsonDataObjects,
  uServices;

type

  [MVCPath('/')]
  TBaseController = class(TMVCController)
  protected
    PageData: TJsonObject;
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); override;
  private
    FGenreService: TGenreService;
    FMovieService: TMovieService;
  strict protected
    function GetGenreService: TGenreService;
    function GetMovieService: TMovieService;
  public
    destructor Destroy; override;
  public
    [MVCPath(''), MVCHTTPMethod([httpGET])]
    [MVCProduces('text/html')]
    procedure Index;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

destructor TBaseController.Destroy;
begin
  if FGenreService <> nil then
    FGenreService.Free;
  if FMovieService <> nil then
    FMovieService.Free;

  inherited;
end;

function TBaseController.GetMovieService: TMovieService;
begin
  if FMovieService = nil then
    FMovieService := TMovieService.Create;
  Result := FMovieService;
end;

function TBaseController.GetGenreService: TGenreService;
begin
  if FGenreService = nil then
    FGenreService := TGenreService.Create;
  Result := FGenreService;
end;

procedure TBaseController.Index;
begin
  LoadView(['header', 'index', 'footer']);
  RenderResponseStream;
end;

procedure TBaseController.OnAfterAction(AContext: TWebContext; const AActionName: string);
begin
  PageData.Free;
  inherited;
end;

procedure TBaseController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  PageData := TJsonObject.Create;
  ViewData['page'] := PageData;
  PageData.S['copyright'] := Format('Copyright %s', [FormatDateTime('yyyy', Now)]);
  PageData.S['version'] := Format('Version %d', [GetFileVersion(ParamStr(0))]);
end;

end.
