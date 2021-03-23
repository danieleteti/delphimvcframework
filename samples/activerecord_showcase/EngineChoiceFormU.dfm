object EngineChoiceForm: TEngineChoiceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'EngineChoiceForm'
  ClientHeight = 281
  ClientWidth = 490
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 484
    Height = 54
    Align = alTop
    Alignment = taCenter
    Caption = 'Choose one of the supported RDBMS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
  end
  object Shape1: TShape
    Left = 24
    Top = 54
    Width = 441
    Height = 3
    Brush.Color = clRed
    Pen.Style = psClear
  end
  object Button1: TButton
    Left = 24
    Top = 82
    Width = 211
    Height = 41
    Caption = 'PostgreSQL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 1
    Left = 24
    Top = 129
    Width = 211
    Height = 41
    Caption = 'Firebird'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Tag = 2
    Left = 24
    Top = 176
    Width = 211
    Height = 41
    Caption = 'Interbase'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button4: TButton
    Tag = 3
    Left = 254
    Top = 82
    Width = 211
    Height = 41
    Caption = 'MSSQLServer'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button5: TButton
    Tag = 4
    Left = 254
    Top = 129
    Width = 211
    Height = 41
    Caption = 'MySQL'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button6: TButton
    Tag = 5
    Left = 254
    Top = 176
    Width = 211
    Height = 41
    Caption = 'MariaDB'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button7: TButton
    Tag = 6
    Left = 24
    Top = 223
    Width = 211
    Height = 41
    Caption = 'SQLite'
    TabOrder = 6
    OnClick = Button1Click
  end
end
