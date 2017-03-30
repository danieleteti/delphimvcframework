# 3.0.0 breaking changes

- ```RenderListAsProperty<T>``` has been removed from TMVCController. You can set such kind of specialized serializations in your custom base controller.
- ```RenderJSONArrayAsProperty``` has been removed from TMVCController. You can set such kind of specialized serializations in your custom base controller.
- ```Render``` has been removed from TMVCController (was deprecated).
- ```Render(TJSONValue)``` has been removed from TMVCController (use Render(TObject)).
- Trying to deserialize a ```TJSONNull``` the target instance will not be freed anymore (consistency with serialize).
- ```Context.Request.BodyAsJSONObject``` dosen't exist any more. Use BodyAs<T> or the following pattern to migrate:
```
JSON := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
try
   if not Assigned(JSON) then
      raise EMVCException.Create('Invalid JSON');
   // do something here
finally
  JSON.Free;
end;
```
- ```TMVCConfigKey``` moved to unit ```MVCFramework.Commons```.
- ```TMVCMimeType``` was renamed to ```TMVCMediaType```.
- ```TMVCController.Render;``` no parameter method do not exist anymore. If the return is a ResponseStream, use the ```RenderResponseStream;```. 
- ```TMVCController.PushJSONToView;``` was renamed to ```PushToView```and Removed SystemJSON dependency, use the ToJSON method if necessary.