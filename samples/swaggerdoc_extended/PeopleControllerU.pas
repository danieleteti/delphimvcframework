unit PeopleControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  BaseControllerU,
  EntitiesU;

type
  [MVCPath('/people')]
  [MVCSWAGDefaultModel(TPersonModel)] {commenting this line you get an exception}
  [MVCSWAGDefaultSummaryTags('People')] {commenting this line you get an exception}
  TPeopleController = class(TBaseController<TPerson, TPersonRec>)
  public
    {this action is defined only in the child controller but uses the "DefaultModel"}
    [MVCPath('/all2')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'List all entities from TPeopleController (child controller, default controller model)', 'getAll2')]
    [MVCSwagResponses(200, 'Success', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAll2; virtual;

    {this action is defined only in the child controller but doesn't use the "DefaultModel"}
    [MVCPath('/all3')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'List all entities from (child controller, custom model)', 'getAll3')]
    [MVCSwagResponses(200, 'Success', TPersonWithNickNameModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAll3; virtual;
  end;


  [MVCPath('/tallpeople')]
  [MVCSWAGDefaultModel(TTallPeople)] {commenting this line you get an exception}
  [MVCSWAGDefaultSummaryTags('Tall People')] {commenting this line you get an exception}
  TTallPeopleController = class(TPeopleController)
  public
    {this action is defined only in the child controller but uses the "DefaultModel"}
    [MVCPath('/all2')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'List all entities from TTallPeopleController (child controller, default controller model)', 'getAll2')]
    [MVCSwagResponses(200, 'Success', SWAGUseDefaultControllerModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAll2; override;

    [MVCPath('/all3')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary(USE_DEFAULT_SUMMARY_TAG, 'List all entities from (child controller, custom model)', 'getAll3')]
    [MVCSwagResponses(200, 'Success', TPersonWithNickNameModel, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAll3; override;
  end;

implementation

uses
  JsonDataObjects,
  System.SysUtils,
  System.DateUtils,
  MVCFramework.Controllers.Register;

{ TPeopleController }

procedure TPeopleController.GetAll2;
begin

end;

procedure TPeopleController.GetAll3;
begin

end;

{ TTallPeopleController }

procedure TTallPeopleController.GetAll2;
begin
  inherited;

end;

procedure TTallPeopleController.GetAll3;
begin
  inherited;

end;

end.
