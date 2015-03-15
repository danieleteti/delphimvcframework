object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'ObjectsMappers SAMPLE 01'
  ClientHeight = 524
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    719
    524)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 703
    Height = 49
    Caption = 'Deserialize complex JSON using the DMVCFramework ObjectsMappers'
    TabOrder = 0
    WordWrap = True
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 63
    Width = 703
    Height = 453
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
