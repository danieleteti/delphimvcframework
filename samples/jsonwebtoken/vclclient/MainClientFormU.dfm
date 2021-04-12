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
    Top = 187
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
    Top = 89
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
    ExplicitTop = 49
  end
  object Memo2: TMemo
    Left = 0
    Top = 190
    Width = 647
    Height = 119
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    ExplicitTop = 150
    ExplicitHeight = 159
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 89
    Align = alTop
    TabOrder = 0
    object btnGet: TButton
      AlignWithMargins = True
      Left = 223
      Top = 4
      Width = 154
      Height = 56
      Align = alLeft
      Caption = 'Get a protected resource'
      TabOrder = 2
      OnClick = btnGetClick
      ExplicitHeight = 41
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 101
      Height = 56
      Align = alLeft
      Caption = 'Login (mode 1)'
      TabOrder = 0
      OnClick = btnLOGINClick
      ExplicitHeight = 41
    end
    object btnLoginWithException: TButton
      AlignWithMargins = True
      Left = 512
      Top = 4
      Width = 131
      Height = 56
      Align = alRight
      Caption = 'Custom Exception in OnAuthenticate'
      TabOrder = 3
      WordWrap = True
      OnClick = btnLoginWithExceptionClick
      ExplicitHeight = 41
    end
    object btnLoginJsonObject: TButton
      AlignWithMargins = True
      Left = 111
      Top = 4
      Width = 106
      Height = 56
      Align = alLeft
      Caption = 'Login (mode 2)'
      TabOrder = 1
      OnClick = btnLoginJsonObjectClick
      ExplicitHeight = 41
    end
    object Panel2: TPanel
      Left = 1
      Top = 63
      Width = 645
      Height = 25
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitTop = 64
      object chkRememberMe: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 3
        Width = 165
        Height = 19
        Margins.Left = 4
        Align = alLeft
        Caption = 'Remember me for longer time'
        TabOrder = 0
      end
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
