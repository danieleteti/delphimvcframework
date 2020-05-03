object EngineChoiceForm: TEngineChoiceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'EngineChoiceForm'
  ClientHeight = 281
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 370
    Height = 66
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
    ExplicitWidth = 353
  end
  object Button1: TButton
    Left = 8
    Top = 90
    Width = 177
    Height = 41
    Caption = 'PostgreSQL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 1
    Left = 8
    Top = 137
    Width = 177
    Height = 41
    Caption = 'Firebird'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Tag = 2
    Left = 8
    Top = 184
    Width = 177
    Height = 41
    Caption = 'Interbase'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button4: TButton
    Tag = 3
    Left = 191
    Top = 90
    Width = 177
    Height = 41
    Caption = 'MSSQLServer'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button5: TButton
    Tag = 4
    Left = 191
    Top = 137
    Width = 177
    Height = 41
    Caption = 'MySQL'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button6: TButton
    Tag = 5
    Left = 191
    Top = 184
    Width = 177
    Height = 41
    Caption = 'MariaDB'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button7: TButton
    Tag = 6
    Left = 8
    Top = 231
    Width = 177
    Height = 41
    Caption = 'SQLite'
    TabOrder = 6
    OnClick = Button1Click
  end
end
