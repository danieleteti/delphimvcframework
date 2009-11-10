object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Receiver Form'
  ClientHeight = 299
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
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 22
    Width = 144
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
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Subscribe'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 320
    Top = 22
    Width = 113
    Height = 25
    Caption = 'Durable Subscribe'
    TabOrder = 2
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 53
    Width = 734
    Height = 238
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
    ExplicitWidth = 586
  end
  object Button3: TButton
    Left = 439
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Unsubscribe'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 520
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button6: TButton
    Left = 158
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 6
    OnClick = Button6Click
  end
end
