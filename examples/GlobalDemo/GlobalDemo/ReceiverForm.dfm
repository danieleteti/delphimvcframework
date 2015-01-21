object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Receiver Form'
  ClientHeight = 291
  ClientWidth = 750
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    750
    291)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 90
    Width = 225
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = '/topic/mytopic'
    TextHint = 'Topic or queue name (/topic/thename or /queue/thename)'
  end
  object Button1: TButton
    Left = 239
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Subscribe'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 320
    Top = 90
    Width = 113
    Height = 25
    Caption = 'Durable Subscribe'
    TabOrder = 2
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 120
    Width = 734
    Height = 163
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
    WordWrap = False
  end
  object Button3: TButton
    Left = 439
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Unsubscribe'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 520
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 5
    OnClick = Button4Click
  end
  object pnlConnection: TPanel
    Left = 8
    Top = 8
    Width = 537
    Height = 57
    TabOrder = 6
    object Button2: TButton
      Left = 441
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = Button2Click
    end
    object edtUserName: TLabeledEdit
      Left = 187
      Top = 19
      Width = 121
      Height = 21
      EditLabel.Width = 49
      EditLabel.Height = 13
      EditLabel.Caption = 'UserName'
      TabOrder = 1
      Text = 'admin'
    end
    object edtPassword: TLabeledEdit
      Left = 314
      Top = 19
      Width = 121
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      TabOrder = 2
      Text = 'password'
    end
    object edtHostNameAndPort: TLabeledEdit
      Left = 8
      Top = 19
      Width = 173
      Height = 21
      EditLabel.Width = 72
      EditLabel.Height = 13
      EditLabel.Caption = 'Hostname:port'
      TabOrder = 3
      Text = 'localhost:61613'
    end
  end
  object ListBox1: TListBox
    Left = 551
    Top = 8
    Width = 191
    Height = 57
    ItemHeight = 13
    TabOrder = 7
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 368
    Top = 152
  end
end
