unit RESTServicesU;

interface

uses
  Generics.Collections, WinesBO, MVCFramework.RESTAdapter,
  MVCFramework.Serializer.Commons, MVCFramework.Commons;

type

  IWineResource = interface(IInvokable)
    ['{068C51B2-F413-48ED-97CE-463234DB3E41}']

    [RESTResource(HttpGet, '/api/wines')]
    [MVCListOf(TWine)]
    [Mapping(TWines)]
    procedure GetWineList(AAsynchReq: IAsynchRequest);

    [RESTResource(httpPOST, '/api/wines')]
    procedure SaveWine([Body] AWine: TWine; AAsynchReq: IAsynchRequest);

    [RESTResource(httpPUT, '/api/wines/{id}')]
    procedure UpdateWineById([Param('id')] AID: integer; [Body] AWine: TWine;
      AAsynchReq: IAsynchRequest);

  end;

implementation

end.
