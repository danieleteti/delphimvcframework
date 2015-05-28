object SvcEEPMServerX: TSvcEEPMServerX
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'SvcEEPMServerX'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
