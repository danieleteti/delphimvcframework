unit Services.OrdersU;

interface

uses EntitiesU, MVCFramework.Repository;

type
  TOrderService = class(TMVCRepository<TOrder>)
  protected
    function Get
  end;

implementation

end.
