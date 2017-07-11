object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JWT with LiveValidityWindow feature'
  ClientHeight = 379
  ClientWidth = 721
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
    Top = 179
    Width = 721
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 147
    ExplicitWidth = 30
  end
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 721
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
    ExplicitTop = 49
    ExplicitWidth = 513
  end
  object Memo2: TMemo
    Left = 0
    Top = 182
    Width = 721
    Height = 197
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    ExplicitTop = 150
    ExplicitWidth = 513
    ExplicitHeight = 229
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 721
    Height = 81
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 338
      Top = 4
      Width = 379
      Height = 73
      Align = alClient
      Caption = 
        'At each authenticated request, the server increments the "exp" p' +
        'roperty of the JWT of LiveValidityWindowInSeconds seconds. So th' +
        'at a JWT doesn'#39't expire never if used at least one time in LiveV' +
        'alidityWindowInSeconds seconds. It is useful to mimic the classi' +
        'c session cookie with the semplicity of the JWT.'
      WordWrap = True
      ExplicitWidth = 368
      ExplicitHeight = 65
    end
    object btnGet: TButton
      AlignWithMargins = True
      Left = 171
      Top = 4
      Width = 161
      Height = 73
      Align = alLeft
      Caption = 'Get a protected resource (with an updated JWT)'
      TabOrder = 0
      WordWrap = True
      OnClick = btnGetClick
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 161
      Height = 73
      Align = alLeft
      Caption = 'Login'
      TabOrder = 1
      OnClick = btnLOGINClick
      ExplicitHeight = 41
    end
  end
end
