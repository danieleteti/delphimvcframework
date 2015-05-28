object IocpHttpTunnel: TIocpHttpTunnel
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'IocpHttpTunnel'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
