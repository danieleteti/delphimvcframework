object ArticlesService: TArticlesService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  AllowPause = False
  DisplayName = 'DMVCFramework Articles RESTServer'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
