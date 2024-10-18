unit Services.RegistrationU;

interface

uses
  MVCFramework.Container;

procedure RegisterServices(Container: IMVCServiceContainer);

implementation

uses
  Services.PeopleU, Services.InterfacesU, Services.ConnectionU;

procedure RegisterServices(Container: IMVCServiceContainer);
begin
  Container.RegisterType(TPeopleService, IPeopleService);
  Container.RegisterType(TConnectionService, IConnectionService, TRegistrationType.SingletonPerRequest)
end;

end.
