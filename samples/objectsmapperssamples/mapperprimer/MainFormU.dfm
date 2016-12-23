object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'ObjectsMapper Samples'
  ClientHeight = 589
  ClientWidth = 1140
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1140
    589)
  PixelsPerInch = 96
  TextHeight = 19
  object Button1: TButton
    Left = 215
    Top = 8
    Width = 201
    Height = 65
    Caption = 'Nested Objects'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 79
    Width = 1124
    Height = 502
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 65
    Caption = 'Mapper Basics'
    TabOrder = 2
    WordWrap = True
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 422
    Top = 8
    Width = 251
    Height = 65
    Caption = 'Nested Objects, JSONName'
    TabOrder = 3
    OnClick = Button3Click
  end
  object btnDatasets: TButton
    Left = 679
    Top = 8
    Width = 202
    Height = 65
    Caption = 'Datasets'
    TabOrder = 4
    OnClick = btnDatasetsClick
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 720
    Top = 120
  end
end
