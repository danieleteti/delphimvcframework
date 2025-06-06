object TestWebModule2: TTestWebModule2
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
    end>
  Height = 230
  Width = 415
end
