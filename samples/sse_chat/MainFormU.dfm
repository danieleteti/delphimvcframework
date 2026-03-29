object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'SSE Chat Client'
  ClientHeight = 500
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblUsername: TLabel
      Left = 8
      Top = 12
      Width = 56
      Height = 15
      Caption = 'Username:'
    end
    object edtUsername: TEdit
      Left = 72
      Top = 9
      Width = 150
      Height = 23
      TabOrder = 0
      Text = 'User1'
    end
    object btnConnect: TButton
      Left = 230
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 312
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = btnDisconnectClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 459
    Width = 700
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object lblTarget: TLabel
      Left = 8
      Top = 13
      Width = 81
      Height = 15
      Caption = 'Broadcast to all'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object edtMessage: TEdit
      Left = 200
      Top = 9
      Width = 416
      Height = 23
      Enabled = False
      TabOrder = 0
      OnKeyPress = edtMessageKeyPress
    end
    object btnSend: TButton
      Left = 622
      Top = 8
      Width = 70
      Height = 25
      Caption = 'Send'
      Enabled = False
      TabOrder = 1
      OnClick = btnSendClick
    end
  end
  object pnlChat: TPanel
    Left = 0
    Top = 41
    Width = 530
    Height = 418
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object memChat: TMemo
      Left = 0
      Top = 0
      Width = 530
      Height = 418
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnlUsers: TPanel
    Left = 530
    Top = 41
    Width = 170
    Height = 418
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object lblOnline: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 0
      Width = 162
      Height = 15
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Online Users'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 70
    end
    object lbUsers: TListBox
      Left = 0
      Top = 19
      Width = 170
      Height = 399
      Align = alClient
      ItemHeight = 15
      TabOrder = 0
      OnClick = lbUsersClick
    end
  end
end
