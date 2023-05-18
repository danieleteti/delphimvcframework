object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'PlaygroundForm'
  ClientHeight = 404
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 49
    Caption = 'lua_pcall'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 159
    Top = 8
    Width = 179
    Height = 49
    Caption = 'Load Config File'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 63
    Width = 515
    Height = 50
    Caption = 'push_object_properties'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 344
    Top = 8
    Width = 179
    Height = 49
    Caption = 'GetGlobal'
    TabOrder = 3
    OnClick = Button4Click
  end
end
