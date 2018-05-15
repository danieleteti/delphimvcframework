unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.TypesAliases,
  MainDataModuleUnit;

type

  [MVCPath('/')]
  TWineCellarApp = class(TMVCController)
  private
    dm: TWineCellarDataModule;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean);
      override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionNAme: string); override;

  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);

    [MVCPath('/wines')]
    [MVCHTTPMethod([httpGET])]
    procedure WinesList(ctx: TWebContext);

    [MVCPath('/wines')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveWine;

    [MVCPath('/wines/search/($value)')]
    procedure FindWines(ctx: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/wines/pdf')]
    procedure GetWinesCatalogAsPDF(ctx: TWebContext);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpGET, httpDELETE])]
    procedure WineById(ctx: TWebContext);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateWineById(id: Integer);
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils,
  WinesBO, MVCFramework.Serializer.Commons;

procedure TWineCellarApp.FindWines(ctx: TWebContext);
begin
  Render(dm.FindWines(ctx.Request.Params['value']));
end;

procedure TWineCellarApp.GetWinesCatalogAsPDF(ctx: TWebContext);
var
  pdf: string;
  PDFFileStream: TFileStream;
  PDFMemoryStream: TMemoryStream;
begin
  pdf := tpath.Combine(AppPath, '..\..\PDFsServedByDMVCBehindApache\Box2DManual.pdf');
  if not TFile.Exists(pdf) then
  begin
    Render(HTTP_STATUS.NotFound, 'File ' + pdf + ' not found');
    exit;
  end;
  PDFFileStream := TFileStream.Create(pdf, fmOpenRead);
  try
    PDFMemoryStream := TMemoryStream.Create;
    try
      PDFMemoryStream.CopyFrom(PDFFileStream, PDFMemoryStream.Size);
    except
      PDFMemoryStream.Free;
      raise;
    end;
  finally
    PDFFileStream.Free;
  end;
  // TFile.Delete(pdf);
  PDFMemoryStream.Position := 0;
  Context.Response.ContentType := 'application/pdf';
  SendStream(PDFMemoryStream);
end;

procedure TWineCellarApp.Index(ctx: TWebContext);
begin
  Redirect('/index.html');
end;

procedure TWineCellarApp.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  inherited;
  dm.Free;
end;

procedure TWineCellarApp.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string;
  var Handled: Boolean);
begin
  inherited;
  dm := TWineCellarDataModule.Create(nil);
end;

procedure TWineCellarApp.SaveWine;
var
  lWine: TWine;
begin
  lWine := Context.Request.BodyAs<TWine>;
  try
    dm.AddWine(lWine);
  finally
    lWine.Free;
  end;
end;

procedure TWineCellarApp.UpdateWineById(id: Integer);
var
  lWine: TWine;
begin
  lWine := Context.Request.BodyAs<TWine>;
  try
    lWine.id := id;
    dm.UpdateWine(lWine);
    Render(TMVCErrorResponse.Create(200, 'Wine Updated', ''));
  finally
    lWine.Free;
  end;
end;

procedure TWineCellarApp.WineById(ctx: TWebContext);
begin
  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      begin
        dm.DeleteWine(StrToInt(ctx.Request.Params['id']));
        Render(200, 'Wine deleted');
      end;
    httpGET:
      begin
        Render(dm.GetWineById(StrToInt(ctx.Request.Params['id'])), False, dstSingleRecord);
      end
  else
    raise Exception.Create('Invalid http method for action');
  end;

end;

procedure TWineCellarApp.WinesList(ctx: TWebContext);
begin
  Render(dm.FindWines(''));
end;

end.
