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
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'List all entities', 'getAll')]
    [MVCSwagResponses(200, 'Success', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAll; virtual;

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'Create an entity', 'createOne')]
    [MVCSwagResponses(201, 'Created')]
    [MVCSwagResponses(500, 'Internal Server Error')]
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
