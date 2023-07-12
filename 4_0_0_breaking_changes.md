## 4.0.0 (oxygen) breaking changes

- Middlewares have been replaced by Filters. There are two kind of filters:

  - **Protocol** Filters (They are called before the routing phase. Handle low level concepts related to the HTTP protocol itself, don't have the notion of "controller" or "action", they can just access the raw `TWebContext`)

  - **Controller** Filters (They are called after the routing phase. Handle much higher level concepts related to the Controllers and Actions. They can access the `TWebContext` but also the selected Controller/Action pair)

- `IMVCMiddleware` interface has been removed. Now there are two different interfaces instead of it (which are not designed to be used directly):

  - IProtocolFilter

    - ```pascal
        IProtocolFilter = interface
          procedure DoFilter(Context:  TWebContext);
          procedure SetNext(NextFilter: IProtocolFilter);
          procedure DoNext(Context: TWebContext);
          procedure OnAfterRegistration(Engine: TMVCEngine);
        end;
      ```

  - IControllerFilter

    - ```pascal
        IControllerFilter = interface
          procedure DoFilter(
            const Context: TWebContext;
            const Router: IMVCRouter);
          procedure SetNext(NextFilter: IControllerFilter);
          procedure SetEngine(const Engine: TMVCEngine);
        end;
      ```

  - These new interfaces are not designed to be used directly (also if it is possible) but they are already implemented in two related custom classes which already provide standard behaviour. These class are the following:

    - TCustomControllerFilter (which implements IControllerFilter)
    - TCustomProtocolFilter (which implements IProtocolFilter)

  - Any filters (protocol or controller related) are ment to call the next one or juts reply to the request by its own. This new architecture aim to a simpler, cleaner and solid codebase for filters (former "middlewares") developers. Nothing change in your application high-level code.

    As a simple example, this is the declaration of a simple middlware before the 4.x version.

    ```pascal
      TMVCSpeedMiddleware = class(TInterfacedObject, IMVCMiddleware)
      public
        procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
        procedure OnAfterControllerAction(Context: TWebContext; const AControllerQualifiedClassName: string;
          const AActionNAme: string; const Handled: Boolean);
        procedure OnBeforeControllerAction(Context: TWebContext; const AControllerQualifiedClassName: string;
          const AActionNAme: string; var Handled: Boolean);
        procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
      end;
    ```

    Now, the equivalent filter is declared as follows:

    ```pascal
      TSpeedProtocolFilter = class(TCustomProtocolFilter)
      protected
        procedure DoFilter(Context: TWebContext); override;
      end;
    ```

  Check samples for more information.

- All units "MVCFramework.Middleware.<Something>.pas" have been renamed to "MVCFramework.Filters.<Something>.pas" and all built-in middlewares have been converted in the prover filter version (controller filter or protocol filter).

- `TMVCEngine.AddMiddleware` has been removed. If you was using built-in middleware you have just to rename `AddMiddleware` to `UseFilter` and use the proper version of the new filter which replaces what previously were named middleware (usually they have the same parameters so it is simple to identify which filter substituted a middleware)

e.g.

```pascal
// if in 3.x you have
  FMVC.AddMiddleware(
  	TMVCActiveRecordMiddleware.Create(DB_CONNECTION_DEF_NAME, ''));

// in 4.0 replace it with the following
  FMVC.UseFilter(
  	TMVCActiveRecordProtocolFilter.Create(DB_CONNECTION_DEF_NAME, ''));

```




|3.x|4.0|
|---|---|
| TMVCActiveRecordMiddleware | TMVCActiveRecordProtocolFilter |
| TCORSMiddleware | TMVCCORSProtocolFilter |
| TMVCCompressionMiddleware | TMVCCompressionProtocolFilter |

TODO...
