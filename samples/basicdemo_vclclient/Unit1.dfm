object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'BasicDemoVCL'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Run BasicDemo.exe first'
  end
  object Button1: TButton
    Left = 8
    Top = 27
    Width = 75
    Height = 25
    Caption = 'Div'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 144
    Width = 117
    Height = 25
    Caption = 'Unicode test'
    TabOrder = 1
    OnClick = Button2Click
  end
end
