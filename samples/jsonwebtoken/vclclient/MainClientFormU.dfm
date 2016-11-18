object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 379
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 147
    Width = 513
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 30
  end
  object Memo1: TMemo
    Left = 0
    Top = 49
    Width = 513
    Height = 98
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 0
    Top = 150
    Width = 513
    Height = 229
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 513
    Height = 49
    Align = alTop
    TabOrder = 2
    object btnGet: TButton
      AlignWithMargins = True
      Left = 171
      Top = 4
      Width = 161
      Height = 41
      Align = alLeft
      Caption = 'Get a protected resource'
      TabOrder = 0
      OnClick = btnGetClick
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 161
      Height = 41
      Align = alLeft
      Caption = 'Login'
      TabOrder = 1
      OnClick = btnLOGINClick
    end
  end
end
