object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 299
  ClientWidth = 413
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
    Top = 8
    Width = 153
    Height = 57
    Caption = 'Load Library'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 167
    Width = 153
    Height = 57
    Caption = 'UnLoad Library'
    TabOrder = 1
    OnClick = Button2Click
  end
  object btnDoSomething: TButton
    Left = 8
    Top = 88
    Width = 153
    Height = 57
    Caption = 'Do Something'
    TabOrder = 2
    OnClick = btnDoSomethingClick
  end
end
