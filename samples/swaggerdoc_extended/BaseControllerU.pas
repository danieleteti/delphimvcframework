unit BaseControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons;

type
  TBaseController<T: class; R: record> = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS,
      'List all ' + TSwaggerConst.PLURAL_MODEL_NAME,
      'getAll' + TSwaggerConst.PLURAL_MODEL_NAME)]
    [MVCSwagResponses(200, 'Success', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error', TMVCErrorResponse)]
    procedure GetAll; virtual;

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary(TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS,
      'Create a ' + TSwaggerConst.SINGULAR_MODEL_NAME,
      'create' + TSwaggerConst.SINGULAR_MODEL_NAME)]
    [MVCSwagResponses(201, 'Created')]
    [MVCSwagResponses(500, 'Internal Server Error', TMVCErrorResponse)]
    [MVCSwagParam(plBody,
      TSwaggerConst.SINGULAR_MODEL_NAME + ' entity',
      'The ' + TSwaggerConst.SINGULAR_MODEL_NAME + ' to create',
      SWAGUseDefaultControllerModel)]
    procedure CreateEntity; virtual;
  end;

implementation

{ TBaseController<T, R> }

procedure TBaseController<T, R>.CreateEntity;
begin

end;

procedure TBaseController<T, R>.GetAll;
begin
  Render(ObjectDict().Add('data',
    StrDict(['prop1','prop2'],['value1','value2']))
    );
end;

end.
