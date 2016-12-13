object ReceiverMainForm: TReceiverMainForm
  Left = 0
  Top = 0
  Anchors = [akLeft, akBottom]
  Caption = 'Receiver Message'
  ClientHeight = 460
  ClientWidth = 477
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
    477
    460)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 66
    Width = 181
    Height = 13
    Caption = 'Message sent from the STOMP broker'
  end
  object Label2: TLabel
    Left = 8
    Top = 342
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Queue Name'
    ExplicitTop = 337
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 449
    Height = 39
    Caption = 
      'This example show how subscribe a queue and after you receive a ' +
      'message from apollo you can send a ACK message to accept the mes' +
      'sage or NACK message to refuse the message. If you refuse the me' +
      'ssage, broker tries to send the message to another subscriber co' +
      'nnected'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 176
    Top = 342
    Width = 95
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Current Message Id'
    ExplicitTop = 337
  end
  object Label5: TLabel
    Left = 8
    Top = 202
    Width = 156
    Height = 13
    Caption = 'Stomp Frame Sent and Received'
  end
  object MessageMemo: TMemo
    Left = 8
    Top = 85
    Width = 431
    Height = 116
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object SubscribeButton: TButton
    Left = 8
    Top = 385
    Width = 129
    Height = 67
    Anchors = [akLeft, akBottom]
    Caption = '1 - Subscribe '
    TabOrder = 1
    OnClick = SubscribeButtonClick
  end
  object QueueEdit: TEdit
    Left = 8
    Top = 358
    Width = 129
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '/queue/alarm'
  end
  object SendAckButton: TButton
    Left = 176
    Top = 385
    Width = 98
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = '2 - Send ACK'
    TabOrder = 3
    OnClick = SendAckButtonClick
  end
  object SendNackButton: TButton
    Left = 176
    Top = 421
    Width = 98
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = '2 - Send NACK'
    TabOrder = 4
    OnClick = SendNackButtonClick
  end
  object UnsubscribeButton: TButton
    Left = 311
    Top = 385
    Width = 129
    Height = 67
    Anchors = [akLeft, akBottom]
    Caption = '3 - Unsubscribe '
    TabOrder = 5
    OnClick = UnsubscribeButtonClick
  end
  object MessageIdEdit: TEdit
    Left = 174
    Top = 358
    Width = 266
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 6
  end
  object LogMemo: TMemo
    Left = 8
    Top = 218
    Width = 431
    Height = 117
    Anchors = [akLeft, akRight, akBottom]
    Color = clInfoBk
    ReadOnly = True
    TabOrder = 7
  end
end
