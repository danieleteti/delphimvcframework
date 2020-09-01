# 3.2.0 (boron) breaking changes

- Removed `TDataSetHelper.LoadFromJSONArrayStringItems`
- `TDataSetHolder` class now renders data in a property called `data` (previously was `items`)
- The default header used by JWT middleware is now `Authorization` (previously was `Authentication`)
- Middleware `OnAfterControllerAction` are now invoked in the same order of `OnBeforeControllerAction` (previously were invoked in reversed order).
- `IMVCMiddleware` has got a new method called after the request processing: `OnAfterRouting` . It is called even if no action is executed. If you don't need it, implement the method and leave it empty.
- `TMVCEngine` is no more responsible for static file serviing. If you need static files used the new `TMVCStaticFilesMiddleware` (check the sample).