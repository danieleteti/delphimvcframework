object fmIocpHttpTunnel: TfmIocpHttpTunnel
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IocpHttpTunnel'
  ClientHeight = 348
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 126
    Width = 384
    Height = 222
    Align = alClient
    TabOrder = 0
    object lbConnections: TLabel
      Left = 136
      Top = 24
      Width = 67
      Height = 13
      Caption = 'lbConnections'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbSentBytes: TLabel
      Left = 136
      Top = 44
      Width = 57
      Height = 13
      Caption = 'lbSentBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbRecvBytes: TLabel
      Left = 136
      Top = 64
      Width = 59
      Height = 13
      Caption = 'lbRecvBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbHandleUsedMemory: TLabel
      Left = 136
      Top = 84
      Width = 103
      Height = 13
      Caption = 'lbHandleUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbHandleFreeMemory: TLabel
      Left = 136
      Top = 104
      Width = 101
      Height = 13
      Caption = 'lbHandleFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbIoUsedMemory: TLabel
      Left = 136
      Top = 124
      Width = 80
      Height = 13
      Caption = 'lbIoUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbIoFreeMemory: TLabel
      Left = 136
      Top = 144
      Width = 78
      Height = 13
      Caption = 'lbIoFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 63
      Height = 13
      Caption = 'Connections:'
    end
    object Label2: TLabel
      Left = 16
      Top = 44
      Width = 56
      Height = 13
      Caption = 'Sent bytes:'
    end
    object Label3: TLabel
      Left = 16
      Top = 64
      Width = 78
      Height = 13
      Caption = 'Received bytes:'
    end
    object Label4: TLabel
      Left = 16
      Top = 84
      Width = 76
      Height = 13
      Caption = 'Handles - Used:'
    end
    object Label5: TLabel
      Left = 16
      Top = 104
      Width = 74
      Height = 13
      Caption = 'Handles - Free:'
    end
    object Label8: TLabel
      Left = 16
      Top = 124
      Width = 50
      Height = 13
      Caption = 'IO - Used:'
    end
    object Label9: TLabel
      Left = 16
      Top = 144
      Width = 48
      Height = 13
      Caption = 'IO - Free:'
    end
    object lbRunTime: TLabel
      Left = 136
      Top = 4
      Width = 49
      Height = 13
      Caption = 'lbRunTime'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 16
      Top = 4
      Width = 43
      Height = 13
      Caption = 'Runtime:'
    end
    object lbSndQueueUsedMemory: TLabel
      Left = 136
      Top = 164
      Width = 120
      Height = 13
      Caption = 'lbSndQueueUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbSndQueueFreeMemory: TLabel
      Left = 136
      Top = 184
      Width = 118
      Height = 13
      Caption = 'lbSndQueueFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 16
      Top = 164
      Width = 95
      Height = 13
      Caption = 'Send queue - Used:'
    end
    object Label16: TLabel
      Left = 16
      Top = 184
      Width = 93
      Height = 13
      Caption = 'Send queue - Free:'
    end
    object Label7: TLabel
      Left = 16
      Top = 204
      Width = 87
      Height = 13
      Caption = 'Pending requests:'
    end
    object lbPendingRequest: TLabel
      Left = 136
      Top = 204
      Width = 86
      Height = 13
      Caption = 'lbPendingRequest'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 126
    Align = alTop
    TabOrder = 1
    object btnStart: TButton
      Left = 16
      Top = 20
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 16
      Top = 51
      Width = 75
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopClick
    end
    object cbConsole: TCheckBox
      Left = 23
      Top = 90
      Width = 65
      Height = 17
      Caption = 'Console'
      TabOrder = 2
      OnClick = cbConsoleClick
    end
    object edtPort: TLabeledEdit
      Left = 112
      Top = 22
      Width = 121
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Port:'
      NumbersOnly = True
      TabOrder = 3
      Text = '80'
    end
    object edtDestHost: TLabeledEdit
      Left = 112
      Top = 60
      Width = 121
      Height = 21
      EditLabel.Width = 82
      EditLabel.Height = 13
      EditLabel.Caption = 'Destination host:'
      TabOrder = 4
    end
    object edtDestPort: TLabeledEdit
      Left = 239
      Top = 60
      Width = 121
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Port:'
      NumbersOnly = True
      TabOrder = 5
      Text = '80'
    end
    object edtTimeout: TLabeledEdit
      Left = 112
      Top = 100
      Width = 121
      Height = 21
      Hint = '0'#34920#31034#19981#38480#21046
      EditLabel.Width = 58
      EditLabel.Height = 13
      EditLabel.Caption = 'Timeout (s):'
      NumbersOnly = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = '0'
    end
    object edtLife: TLabeledEdit
      Left = 239
      Top = 100
      Width = 121
      Height = 21
      Hint = '0'#34920#31034#19981#38480#21046
      EditLabel.Width = 37
      EditLabel.Height = 13
      EditLabel.Caption = 'Life (s):'
      NumbersOnly = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = '0'
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 216
    Top = 136
  end
end
