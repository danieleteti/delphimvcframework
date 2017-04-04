object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Delphi Stomp Client - CHAT DEMO - www.danieleteti.it'
  ClientHeight = 435
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    511
    435)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 7
    Top = 10
    Width = 137
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object Edit2: TEdit
    Left = 150
    Top = 10
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'ChatRoomName'
  end
  object Button1: TButton
    Left = 404
    Top = 8
    Width = 99
    Height = 25
    Caption = 'Enter'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit3: TEdit
    Left = 277
    Top = 10
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'daniele_teti'
  end
  object Memo1: TMemo
    Left = 9
    Top = 39
    Width = 494
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clMenuBar
    Font.Charset = ANSI_CHARSET
    Font.Color = clHotLight
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object Memo2: TMemo
    Left = 8
    Top = 359
    Width = 389
    Height = 68
    Anchors = [akLeft, akRight, akBottom]
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnKeyUp = Memo2KeyUp
  end
  object Button2: TButton
    Left = 403
    Top = 359
    Width = 100
    Height = 68
    Anchors = [akRight, akBottom]
    Caption = 'Send'
    Enabled = False
    TabOrder = 6
    OnClick = Button2Click
  end
  object tmr: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrTimer
    Left = 432
    Top = 72
  end
end
