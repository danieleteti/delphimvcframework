unit Controllers.Base;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  TBaseController = class abstract(TMVCController)
  strict protected
  public
    destructor Destroy; override;
  end;

  [MVCPath('/private')]
  TPrivateController = class(TBaseController)
  public
    [MVCPath('/articles')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeleteAllArticles;
  end;

implementation

uses
  System.SysUtils;

{ TBaseController }

destructor TBaseController.Destroy;
begin
  inherited;
end;

{ TPrivateController }

procedure TPrivateController.DeleteAllArticles;
begin
  raise EMVCException.Create(HTTP_STATUS.NotImplemented, 'Not Implemented');
end;

end.
