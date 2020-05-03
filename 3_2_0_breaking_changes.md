# 3.2.0 (boron) breaking changes

- Removed `TDataSetHelper.LoadFromJSONArrayStringItems`
- `TDataSetHolder` class now renders data in a property called `data` (previously was `items`)
- The default header used by JWT middleware is now `Authorization` (previously was `Authentication`)
- Middleware `OnAfterControllerAction` are now invoked in the same order of `OnBeforeControllerAction` (previously were invoked in reversed order).