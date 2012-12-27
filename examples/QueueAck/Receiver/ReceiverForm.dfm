object ReceiverMainForm: TReceiverMainForm
  Left = 0
  Top = 0
  Caption = 'Receiver Message'
  ClientHeight = 455
  ClientWidth = 478
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
  object Label1: TLabel
    Left = 8
    Top = 66
    Width = 122
    Height = 13
    Caption = 'Message sent from apollo'
  end
  object Label2: TLabel
    Left = 8
    Top = 337
    Width = 62
    Height = 13
    Caption = 'Queue Name'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 396
    Height = 52
    Caption = 
      'This example show how subscribe a queue and after you receive a ' +
      'message from apollo you can send a ACK message to accept the mes' +
      'sage or NACK message to refuse the message. If you refuse the me' +
      'ssage apollo try to send the message to another subscriber conne' +
      'cted'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 176
    Top = 337
    Width = 95
    Height = 13
    Caption = 'Current Message Id'
  end
  object Label5: TLabel
    Left = 8
    Top = 202
    Width = 150
    Height = 13
    Caption = 'Stomp Frame Sent and Receive'
  end
  object MessageMemo: TMemo
    Left = 8
    Top = 85
    Width = 432
    Height = 116
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object SubscribeButton: TButton
    Left = 8
    Top = 380
    Width = 129
    Height = 67
    Caption = '1 - Subscribe '
    TabOrder = 1
    OnClick = SubscribeButtonClick
  end
  object QueueEdit: TEdit
    Left = 8
    Top = 353
    Width = 129
    Height = 21
    TabOrder = 2
    Text = '/queue/alarm'
  end
  object SendAckButton: TButton
    Left = 176
    Top = 380
    Width = 98
    Height = 31
    Caption = '2 - Send ACK'
    TabOrder = 3
    OnClick = SendAckButtonClick
  end
  object SendNackButton: TButton
    Left = 176
    Top = 416
    Width = 98
    Height = 31
    Caption = '2 - Send NACK'
    TabOrder = 4
    OnClick = SendNackButtonClick
  end
  object UnsubscribeButton: TButton
    Left = 311
    Top = 380
    Width = 129
    Height = 67
    Caption = '3 - Unsubscribe '
    TabOrder = 5
    OnClick = UnsubscribeButtonClick
  end
  object MessageIdEdit: TEdit
    Left = 174
    Top = 353
    Width = 266
    Height = 21
    TabOrder = 6
  end
  object LogMemo: TMemo
    Left = 8
    Top = 218
    Width = 410
    Height = 117
    TabOrder = 7
  end
end
