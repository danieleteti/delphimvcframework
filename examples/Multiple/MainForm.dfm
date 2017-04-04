object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'STOMP Listener'
  ClientHeight = 506
  ClientWidth = 527
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
    527
    506)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 279
    Top = 8
    Width = 117
    Height = 42
    Caption = 'Start subscriber'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 511
    Height = 442
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Button2: TButton
    Left = 402
    Top = 8
    Width = 117
    Height = 42
    Caption = 'Stop subscriber'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 42
    Anchors = [akTop, akRight]
    Caption = 'Produce messages continuosly on a background thread'
    TabOrder = 3
    WordWrap = True
    OnClick = Button3Click
  end
end
