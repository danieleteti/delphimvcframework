object MainWebModule: TMainWebModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
    end>
  Height = 150
  Width = 215
end
