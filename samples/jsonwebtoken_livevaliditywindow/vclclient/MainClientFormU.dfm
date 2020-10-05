object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JWT with LiveValidityWindow feature'
  ClientHeight = 379
  ClientWidth = 908
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
    Width = 908
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 147
    ExplicitWidth = 30
  end
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 908
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
    Top = 182
    Width = 908
    Height = 197
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
    Width = 908
    Height = 81
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 540
      Top = 4
      Width = 364
      Height = 73
      Align = alClient
      Caption = 
        'At each authenticated request, the server increments the "exp" p' +
        'roperty of the JWT of LiveValidityWindowInSeconds seconds. So th' +
        'at a JWT doesn'#39't expire never if used at least one time in LiveV' +
        'alidityWindowInSeconds seconds. It is useful to mimic the classi' +
        'c session cookie with the semplicity of the JWT.'
      WordWrap = True
      ExplicitWidth = 355
      ExplicitHeight = 65
    end
    object btnGet: TButton
      AlignWithMargins = True
      Left = 373
      Top = 4
      Width = 161
      Height = 73
      Align = alLeft
      Caption = 'Get a protected resource (with an updated JWT)'
      TabOrder = 3
      WordWrap = True
      OnClick = btnGetClick
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 117
      Height = 73
      Align = alLeft
      Caption = 'Login (Custom headers)'
      TabOrder = 0
      WordWrap = True
      OnClick = btnLOGINClick
    end
    object btnLoginWithHeaderBasic: TButton
      AlignWithMargins = True
      Left = 127
      Top = 4
      Width = 117
      Height = 73
      Align = alLeft
      Caption = 'Login (with Basic Schema)'
      TabOrder = 1
      WordWrap = True
      OnClick = btnLoginWithHeaderBasicClick
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 250
      Top = 4
      Width = 117
      Height = 73
      Align = alLeft
      Caption = 'Login (with JSON Object)'
      TabOrder = 2
      WordWrap = True
      OnClick = Button1Click
    end
  end
end
