object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JWT Authentication Client - DMVCFramework'
  ClientHeight = 620
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 780
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object gbLogin: TGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 380
      Height = 129
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alLeft
      Caption = ' Login '
      TabOrder = 0
      object lblUser: TLabel
        Left = 16
        Top = 24
        Width = 56
        Height = 15
        Caption = 'Username:'
      end
      object lblPass: TLabel
        Left = 16
        Top = 56
        Width = 54
        Height = 15
        Caption = 'Password:'
      end
      object edtUsername: TEdit
        Left = 88
        Top = 21
        Width = 140
        Height = 23
        TabOrder = 0
        Text = 'user1'
      end
      object edtPassword: TEdit
        Left = 88
        Top = 53
        Width = 140
        Height = 23
        PasswordChar = '*'
        TabOrder = 1
        Text = 'user1'
      end
      object btnLogin: TButton
        Left = 248
        Top = 20
        Width = 120
        Height = 25
        Caption = 'Login (Headers)'
        TabOrder = 2
        OnClick = btnLoginClick
      end
      object btnLoginJSON: TButton
        Left = 248
        Top = 52
        Width = 120
        Height = 25
        Caption = 'Login (JSON Body)'
        TabOrder = 3
        OnClick = btnLoginJSONClick
      end
      object chkRememberMe: TCheckBox
        Left = 88
        Top = 88
        Width = 170
        Height = 17
        Caption = 'Remember me (10h token)'
        TabOrder = 4
      end
    end
    object gbActions: TGroupBox
      AlignWithMargins = True
      Left = 396
      Top = 8
      Width = 376
      Height = 129
      Margins.Left = 4
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = ' Protected Resources '
      TabOrder = 1
      object btnGetRole1: TButton
        Left = 16
        Top = 28
        Width = 150
        Height = 25
        Caption = 'GET /admin/role1'
        Enabled = False
        TabOrder = 0
        OnClick = btnGetRole1Click
      end
      object btnGetRole2: TButton
        Left = 16
        Top = 60
        Width = 150
        Height = 25
        Caption = 'GET /admin/role2'
        Enabled = False
        TabOrder = 1
        OnClick = btnGetRole2Click
      end
      object btnGetPublic: TButton
        Left = 184
        Top = 28
        Width = 170
        Height = 25
        Caption = 'GET /public (no auth)'
        TabOrder = 2
        OnClick = btnGetPublicClick
      end
      object btnLoginException: TButton
        Left = 184
        Top = 60
        Width = 170
        Height = 25
        Caption = 'Login with Exception'
        TabOrder = 3
        OnClick = btnLoginExceptionClick
      end
      object btnClear: TButton
        Left = 184
        Top = 92
        Width = 170
        Height = 25
        Caption = 'Clear Log'
        TabOrder = 4
        OnClick = btnClearClick
      end
    end
  end
  object gbToken: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 149
    Width = 764
    Height = 113
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 4
    Align = alTop
    Caption = ' JWT Token '
    TabOrder = 1
    object mmoToken: TMemo
      AlignWithMargins = True
      Left = 6
      Top = 20
      Width = 752
      Height = 87
      Margins.Left = 4
      Margins.Top = 2
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Color = clInfoBk
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
  end
  object gbResponse: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 270
    Width = 764
    Height = 322
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 4
    Align = alClient
    Caption = ' Server Response '
    TabOrder = 2
    object mmoResponse: TMemo
      AlignWithMargins = True
      Left = 6
      Top = 20
      Width = 752
      Height = 296
      Margins.Left = 4
      Margins.Top = 2
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 596
    Width = 780
    Height = 24
    Panels = <
      item
        Text = 'Not authenticated'
        Width = 250
      end
      item
        Text = 'Server: localhost:8080'
        Width = 200
      end>
  end
end
