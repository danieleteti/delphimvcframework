object FormWebSocketChat: TFormWebSocketChat
  Left = 0
  Top = 0
  Caption = 'DMVC WebSocket Chat Client'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 97
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 80
      Height = 13
      Caption = 'WebSocket URL:'
    end
    object EditURL: TEdit
      Left = 16
      Top = 35
      Width = 337
      Height = 21
      TabOrder = 0
      Text = 'ws://localhost:9091/'
    end
    object ButtonConnect: TButton
      Left = 368
      Top = 33
      Width = 120
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = ButtonConnectClick
    end
    object ButtonDisconnect: TButton
      Left = 488
      Top = 33
      Width = 120
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = ButtonDisconnectClick
    end
    object CheckAutoReconnect: TCheckBox
      Left = 16
      Top = 62
      Width = 153
      Height = 17
      Caption = 'Auto-Reconnect'
      TabOrder = 3
    end
    object ButtonPing: TButton
      Left = 488
      Top = 62
      Width = 120
      Height = 25
      Caption = 'Send Ping'
      Enabled = False
      TabOrder = 4
      OnClick = ButtonPingClick
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 97
    Width = 624
    Height = 288
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 385
    Width = 624
    Height = 56
    Align = alBottom
    TabOrder = 2
    object EditMessage: TEdit
      Left = 16
      Top = 16
      Width = 457
      Height = 21
      Enabled = False
      TabOrder = 0
      OnKeyPress = EditMessageKeyPress
    end
    object ButtonSend: TButton
      Left = 488
      Top = 14
      Width = 120
      Height = 25
      Caption = 'Send'
      Enabled = False
      TabOrder = 1
      OnClick = ButtonSendClick
    end
  end
end
