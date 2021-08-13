object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JWT Blacklisting - Client Sample'
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
  object Splitter2: TSplitter
    Left = 0
    Top = 185
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
    Height = 136
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    ExplicitTop = 65
  end
  object Memo2: TMemo
    Left = 0
    Top = 188
    Width = 647
    Height = 272
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    ExplicitTop = 166
    ExplicitHeight = 143
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
      Left = 250
      Top = 4
      Width = 154
      Height = 41
      Align = alLeft
      Caption = 'Get a protected resource'
      TabOrder = 1
      OnClick = btnGetClick
      ExplicitLeft = 111
      ExplicitHeight = 57
    end
    object btnLOGIN: TButton
      AlignWithMargins = True
      Left = 143
      Top = 4
      Width = 101
      Height = 41
      Align = alLeft
      Caption = 'Login'
      TabOrder = 0
      OnClick = btnLOGINClick
      ExplicitLeft = 4
      ExplicitHeight = 57
    end
    object btnLogout: TButton
      AlignWithMargins = True
      Left = 410
      Top = 4
      Width = 194
      Height = 41
      Align = alLeft
      Caption = 'Ask the server to put my JWT in a blacklist (a.k.a. JWT LOGOUT)'
      TabOrder = 2
      WordWrap = True
      OnClick = btnLogoutClick
      ExplicitLeft = 271
      ExplicitTop = 2
    end
    object btnPublicResource: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 133
      Height = 41
      Align = alLeft
      Caption = 'Get a public resource'
      TabOrder = 3
      OnClick = btnPublicResourceClick
    end
  end
end
