unit DIConfiguration;

interface

uses
  Spring, Spring.Container;

procedure BuildContainer;
function Container: TContainer;

implementation

uses Spring.Container.Common, Controller1U, Service1U, Service2U,
  ServicesInterfaceU, Controller2U, Service3U;

var
  GContainer: TContainer = nil;

function Container: TContainer;
begin
  Result := GContainer;
end;

procedure BuildContainer;
begin
  Assert(not Assigned(GContainer), 'Container already built');
  GContainer := TContainer.Create;
  // Registering controllers
  GContainer.RegisterType<TMyController1>;
  GContainer.RegisterType<TMyController2>;

  // Registering Services
  GContainer.RegisterType<TUsersService>.Implements<IUsersService>;
  GContainer.RegisterType<TCustomersService>.Implements<ICustomersService>;

  // Maybe that the common service must be register as singleton or as singleton per thread
  // because must be the same instances between the first and the second service (e.g. DB transaction)

  // Transient registration (default)
  // GContainer.RegisterType<TCommonService>.Implements<ICommonService>;

  // Singleton registration for all thread. WARNING!!! It is shared between HTTP calls.
  // GContainer.RegisterType<TCommonService>.Implements<ICommonService>.AsSingleton(TRefCounting.True);

  // Singleton per thread registration. WARNING!!! (read below) Shared by all services within the same HTTP call.
  // GContainer.RegisterType<TCommonService>.Implements<ICommonService>.AsSingletonPerThread(TRefCounting.True);

  {
    About "AsSingletonPerThread" Stefan Glienke said:
    It might be confusing as people are assuming that the container magically
    knows when a thread ends to destroy a singleton per thread. But that is not the case.
    In fact it gets created once per threadid. That means even if your thread has
    ended and a new one starts later using the same threadid you get the
    same object as before.
    If you don't use a threadpool where you have the same threads running all the
    time performing tasks it is not a good idea to use singleton per thread.
    You might use transient then to always create a new instance or - and this
    is imo the better solution - use a threadpool which limits the objects
    created (and also reduce the creation of thread objects).
    Nevertheless singleton per thread instances will always be destroyed
    when the container is getting destroyed, not earlier.
    This is also not a strange behavior of our container but also Castle Windsor
    or Unity. However there is also advice against using it there.
    If you are looking more for something like singleton per request I suggest
    using transient and making sure that at the start of the request it gets
    resolved from the container and then passed around where ever needed (think
    about implementing refcounting to your data module!) and dropping it at the
    end of the request which makes it getting destroyed due to ref counting.
  }

  // Build the container
  GContainer.Build;
end;

initialization

finalization

GContainer.Free;

end.
