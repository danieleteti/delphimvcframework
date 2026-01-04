unit RegistrationU;

interface

uses
  MVCFramework.Container;

procedure RegisterServices(Container: IMVCServiceContainer);

implementation

uses
  MVCFramework.Repository, EntitiesU;

procedure RegisterServices(Container: IMVCServiceContainer);
begin
  Container.RegisterType(TMVCRepository<TCustomer>, IMVCRepository<TCustomer>, TRegistrationType.SingletonPerRequest);
end;

end.
