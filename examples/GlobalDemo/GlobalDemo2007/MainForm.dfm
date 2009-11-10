object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Sender Form'
  ClientHeight = 182
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 77
    Width = 193
    Height = 21
    TabOrder = 0
    Text = '/topic/mytopic'
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Begin Trans'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Abort Trans'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Commit Trans'
    TabOrder = 3
    OnClick = Button4Click
  end
  object chkPersistent: TCheckBox
    Left = 8
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Persistent'
    TabOrder = 4
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 417
    Height = 69
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object Button6: TButton
    Left = 431
    Top = 104
    Width = 111
    Height = 41
    Caption = 'Send'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button1: TButton
    Left = 431
    Top = 149
    Width = 111
    Height = 25
    Caption = 'Send x 10'
    TabOrder = 7
    OnClick = Button1Click
  end
end
