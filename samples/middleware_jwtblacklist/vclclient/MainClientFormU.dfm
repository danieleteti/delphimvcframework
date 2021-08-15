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
    Top = 217
    Width = 647
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = -8
    ExplicitTop = 302
    ExplicitWidth = 513
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
  object Panel2: TPanel
    Left = 0
    Top = 49
    Width = 647
    Height = 168
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 639
      Height = 13
      Align = alTop
      Caption = 'Current Token'
      ExplicitWidth = 69
    end
    object MemoJWT: TMemo
      Left = 1
      Top = 20
      Width = 645
      Height = 147
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 220
    Width = 647
    Height = 240
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 2
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 639
      Height = 13
      Align = alTop
      Caption = 'Raw Response'
      ExplicitWidth = 71
    end
    object MemoRawResponse: TMemo
      Left = 1
      Top = 20
      Width = 645
      Height = 219
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
end
