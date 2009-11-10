object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Sender Form'
  ClientHeight = 139
  ClientWidth = 473
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
    Top = 37
    Width = 193
    Height = 21
    TabOrder = 0
    Text = '/topic/mytopic'
    TextHint = 'Topic or queue name (/topic/thename or /queue/thename)'
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
    Left = 251
    Top = 12
    Width = 97
    Height = 17
    Caption = 'Persistent'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object Memo1: TMemo
    Left = 8
    Top = 64
    Width = 340
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
    Left = 354
    Top = 64
    Width = 111
    Height = 69
    Caption = 'Send'
    TabOrder = 6
    OnClick = Button6Click
  end
end
