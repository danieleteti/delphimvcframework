object SendMessageMainForm: TSendMessageMainForm
  Left = 0
  Top = 0
  Caption = 'Send Message'
  ClientHeight = 385
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    612
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 318
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Queue Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 5
    Width = 182
    Height = 13
    Caption = 'Type here text of message to be sent'
  end
  object Label3: TLabel
    Left = 8
    Top = 127
    Width = 150
    Height = 13
    Caption = 'Stomp Frame Sent and Receive'
  end
  object QueueMemo: TMemo
    Left = 8
    Top = 24
    Width = 596
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 410
  end
  object SendMessageButton: TButton
    Left = 8
    Top = 320
    Width = 138
    Height = 56
    Anchors = [akLeft, akBottom]
    Caption = 'Send Message'
    TabOrder = 1
    OnClick = SendMessageButtonClick
    ExplicitTop = 269
  end
  object QueueEdit: TEdit
    Left = 152
    Top = 337
    Width = 266
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '/queue/alarm'
  end
  object LogMemo: TMemo
    Left = 8
    Top = 146
    Width = 596
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object AutomaticSendCheckBox: TCheckBox
    Left = 152
    Top = 360
    Width = 181
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Automatic send every 5 seconds '
    TabOrder = 4
    OnClick = AutomaticSendCheckBoxClick
  end
  object AutomaticSendTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = AutomaticSendTimerTimer
    Left = 320
    Top = 24
  end
end
