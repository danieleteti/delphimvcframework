object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 460
  ClientWidth = 647
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
    Top = 309
    Width = 647
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -16
    ExplicitTop = 302
    ExplicitWidth = 513
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 147
    Width = 647
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = -8
    ExplicitTop = 302
    ExplicitWidth = 513
  end
  object Memo1: TMemo
    Left = 0
    Top = 49
    Width = 647
    Height = 98
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 0
    Top = 150
    Width = 647
    Height = 159
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 49
    Align = alTop
    TabOrder = 0
    object btnGet: TButton
      AlignWithMargins = True
      Left = 223
      Top = 4
      Width = 154
      Height = 41
      Align = alLeft
      Caption = 'Get a protected resource'
      TabOrder = 2
      OnClick = btnGetClick
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 101
      Height = 41
      Align = alLeft
      Caption = 'Login (mode 1)'
      TabOrder = 0
      OnClick = btnLOGINClick
    end
    object btnLoginWithException: TButton
      AlignWithMargins = True
      Left = 512
      Top = 4
      Width = 131
      Height = 41
      Align = alRight
      Caption = 'Custom Exception in OnAuthenticate'
      TabOrder = 3
      WordWrap = True
      OnClick = btnLoginWithExceptionClick
    end
    object btnLoginJsonObject: TButton
      AlignWithMargins = True
      Left = 111
      Top = 4
      Width = 106
      Height = 41
      Align = alLeft
      Caption = 'Login (mode 2)'
      TabOrder = 1
      OnClick = btnLoginJsonObjectClick
    end
  end
  object Memo3: TMemo
    Left = 0
    Top = 312
    Width = 647
    Height = 148
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
  end
end
