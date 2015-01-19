unit RESTServicesU;

interface

uses
  Generics.Collections, WinesBO, MVCFramework.RESTAdapter, MVCFramework,
  ObjectsMappers;

type

  IWineResource = interface(IInvokable)
    ['{068C51B2-F413-48ED-97CE-463234DB3E41}']

    [RESTResource(HttpGet, '/wines')]
    [MapperListOf(TWine)]
    function GetWineList: TObjectList<TWine>;

    [RESTResource(httpPOST, '/wines')]
    procedure SaveWine([Body] AWine: TWine);

    [RESTResource(httpPUT, '/wines/{id}')]
    procedure UpdateWineById([Param('id')] AID: integer; [Body] AWine: TWine);

  end;

implementation

end.
