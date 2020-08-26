object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'JWT Sample'
  ClientHeight = 160
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 87
    Width = 145
    Height = 66
    Caption = 'Get JWT Token'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 159
    Top = 87
    Width = 338
    Height = 66
    Caption = 'Get Request to a protected resources'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 505
    Height = 81
    Align = alTop
    ReadOnly = True
    TabOrder = 0
  end
end
