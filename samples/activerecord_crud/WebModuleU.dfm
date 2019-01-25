object MyWebModule: TMyWebModule
  OldCreateOrder = False
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
    Left = 200
    Top = 104
  end
end
