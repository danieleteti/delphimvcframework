object frmSimpleSwaggerDocDemo: TfrmSimpleSwaggerDocDemo
  Left = 0
  Top = 0
  Caption = 'Load Swagger.json to SwagDoc'
  ClientHeight = 214
  ClientWidth = 876
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblApiDescription: TLabel
    Left = 102
    Top = 8
    Width = 6
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 39
    Width = 876
    Height = 175
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnLoadJSON: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load JSON'
    TabOrder = 1
    OnClick = btnLoadJSONClick
  end
end
