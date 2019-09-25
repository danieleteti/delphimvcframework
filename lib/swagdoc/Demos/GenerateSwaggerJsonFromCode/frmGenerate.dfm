object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Generate Swagger JSON'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 47
    Width = 635
    Height = 252
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnGenerate: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 1
    OnClick = btnGenerateClick
  end
end
