object MyWebModule: TMyWebModule
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <>
  Height = 230
  Width = 415
  object FDQuery1: TFDQuery
    Left = 192
    Top = 96
  end
  object FDConnection1: TFDConnection
    Left = 288
    Top = 48
  end
end
