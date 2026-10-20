# DMVCFramework — DI container and Repository pattern

The service container (registration, the three injection points) and `IMVCRepository<T>`.

---

## Service Container (DI Pattern)

```delphi
// services.pas
type
  IMyService = interface
    ['{YOUR-GUID-HERE}']
    function DoSomething: string;
  end;

  TMyService = class(TInterfacedObject, IMyService)
    function DoSomething: string;
  end;

procedure RegisterServices(Container: IMVCServiceContainer);
begin
  Container.RegisterType(TMyService, IMyService, TRegistrationType.SingletonPerRequest);
end;
```

**Registration happens ONCE in the `.dpr`, before the server starts — never per request.**
`TWebContext` exposes only a read-only `ServiceContainerResolver`; it has **no** `ServiceContainer` property.

```delphi
// .dpr — before RunServer
RegisterServices(DefaultMVCServiceContainer);
DefaultMVCServiceContainer.Build;          // mandatory: Build seals the container
```

Three injection points (all used by the samples):

```delphi
// 1. Constructor injection — preferred for a controller's main collaborator
type
  [MVCPath('/customers')]
  TCustomersController = class(TMVCController)
  private
    fRepo: IMVCRepository<TCustomer>;
  public
    [MVCInject]
    constructor Create(CustomersRepository: IMVCRepository<TCustomer>); reintroduce;
  end;

// 2. Action-parameter injection — for a service used by one action only
[MVCPath] [MVCHTTPMethods([httpGET])]
function GetCustomers(
  [MVCFromQueryString('rql', '')] RQLFilter: String;
  [MVCInject] CustomersService: ICustomersService): IMVCResponse;

// 3. Field injection
[MVCInject] fMyService: IMyService;
```

`TRegistrationType`: `Transient` | `Singleton` | `SingletonPerRequest`.

---


---

## Repository Pattern (`MVCFramework.Repository`)

Alternative to calling `TMVCActiveRecord` class methods straight from the controller: an injectable
per-entity repository. Use it when the controller should not know about persistence, or when you
need custom finders behind an interface.

`IMVCRepository<T: TMVCActiveRecord, constructor>` — ready-made members:

| Group | Members |
|-------|---------|
| CRUD | `GetByPK(Int64\|string\|TGuid; RaiseExceptionIfNotFound=True)`, `Insert`, `Update`, `Delete`, `Store` |
| Query | `GetAll`, `GetWhere(SQLWhere, Params[, ParamTypes])`, `GetOneByWhere`, `GetFirstByWhere` |
| RQL | `SelectRQL(RQL, MaxRecordCount=1000)`, `GetOneByRQL`, `CountRQL`, `DeleteRQL` |
| SQL | `Select(SQL, Params[, ParamTypes])`, `SelectOne` |
| Named queries | `SelectByNamedQuery`, `SelectRQLByNamedQuery`, `CountRQLByNamedQuery`, `DeleteRQLByNamedQuery` |
| Utility | `Count(RQL='')`, `DeleteAll`, `Exists(Int64\|string\|TGuid)` |

**Plain use — no custom repo class needed:**

```delphi
// registration (.dpr)
Container.RegisterType(TMVCRepository<TCustomer>, IMVCRepository<TCustomer>,
  TRegistrationType.SingletonPerRequest);

// controller
[MVCInject]
constructor Create(CustomersRepository: IMVCRepository<TCustomer>); reintroduce;
```

**Custom repository — extend the interface when you need domain finders:**

```delphi
type
  ICustomerRepository = interface(IMVCRepository<TCustomer>)
    ['{969BC9CE-....}']
    function GetTopRatedCustomers: TObjectList<TCustomer>;
  end;

  TCustomerRepository = class(TMVCRepository<TCustomer>, ICustomerRepository)
  public
    function GetTopRatedCustomers: TObjectList<TCustomer>;
  end;

function TCustomerRepository.GetTopRatedCustomers: TObjectList<TCustomer>;
begin
  Result := SelectByNamedQuery('BestCustomers', [], []);   // inherited helper
end;
```

**Transactions — the inline guard is the idiom (works for both AR and Repository):**

```delphi
function TCustomersController.DeleteCustomer(const ID: Integer): IMVCResponse;
begin
  var lTx := TMVCRepository.UseTransactionContext;   // commits on scope exit, rolls back on exception
  var lCustomer := fCustomersRepository.GetByPK(ID);
  fCustomersRepository.Delete(lCustomer);
  Result := NoContentResponse;
end;
```

`TMVCActiveRecord.UseTransactionContext` is the equivalent when not using repositories.
`TMVCRepositoryWithConnection<T>` binds a repository to a specific connection name.

---
