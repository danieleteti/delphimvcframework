object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'DMVCFramework WebSocket Client Demo'
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 143
    Height = 15
    Caption = 'WebSocket Server (Python)'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 62
    Height = 15
    Caption = 'Your Name:'
  end
  object Label3: TLabel
    Left = 16
    Top = 168
    Width = 54
    Height = 15
    Caption = 'Messages:'
  end
  object lblStatus: TLabel
    Left = 16
    Top = 472
    Width = 76
    Height = 15
    Caption = 'Disconnected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtServerURL: TEdit
    Left = 16
    Top = 37
    Width = 521
    Height = 23
    TabOrder = 0
    Text = 'ws://localhost:8765'
  end
  object btnConnect: TButton
    Left = 552
    Top = 35
    Width = 130
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object edtName: TEdit
    Left = 16
    Top = 85
    Width = 521
    Height = 23
    TabOrder = 2
    Text = 'Daniele'
  end
  object btnSendName: TButton
    Left = 552
    Top = 83
    Width = 130
    Height = 25
    Caption = 'Send Name'
    Enabled = False
    TabOrder = 3
    OnClick = btnSendNameClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 189
    Width = 666
    Height = 268
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnDisconnect: TButton
    Left = 552
    Top = 128
    Width = 130
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 5
    OnClick = btnDisconnectClick
  end
  object btnClear: TButton
    Left = 16
    Top = 128
    Width = 130
    Height = 25
    Caption = 'Clear Log'
    TabOrder = 6
    OnClick = btnClearClick
  end
end
