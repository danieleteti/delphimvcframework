object EngineChoiceForm: TEngineChoiceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'EngineChoiceForm'
  ClientHeight = 263
  ClientWidth = 521
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 5
    Width = 515
    Height = 33
    Margins.Top = 5
    Align = alTop
    Alignment = taCenter
    Caption = 'Choose one of the 8 supported RDBMS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
    ExplicitTop = 4
    ExplicitWidth = 522
  end
  object Shape1: TShape
    Left = 24
    Top = 47
    Width = 477
    Height = 3
    Margins.Top = 5
    Brush.Color = clRed
    Pen.Style = psClear
  end
  object Button1: TButton
    Left = 27
    Top = 66
    Width = 232
    Height = 41
    Caption = 'PostgreSQL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 1
    Left = 27
    Top = 113
    Width = 232
    Height = 41
    Caption = 'Firebird'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Tag = 2
    Left = 27
    Top = 160
    Width = 232
    Height = 41
    Caption = 'Interbase'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button4: TButton
    Tag = 3
    Left = 265
    Top = 66
    Width = 232
    Height = 41
    Caption = 'MSSQLServer'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button5: TButton
    Tag = 4
    Left = 265
    Top = 113
    Width = 232
    Height = 41
    Caption = 'MySQL'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button6: TButton
    Tag = 5
    Left = 265
    Top = 160
    Width = 232
    Height = 41
    Caption = 'MariaDB'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button7: TButton
    Tag = 6
    Left = 27
    Top = 207
    Width = 232
    Height = 41
    Caption = 'SQLite'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button8: TButton
    Tag = 7
    Left = 265
    Top = 207
    Width = 232
    Height = 41
    Caption = 'Oracle'
    TabOrder = 7
    OnClick = Button1Click
  end
end
