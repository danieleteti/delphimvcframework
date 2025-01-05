object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Data Pump With MVCActiveRecord Sample'
  ClientHeight = 64
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 211
    Height = 15
    Caption = 'Migrate data from SQLite to PostgreSQL'
  end
  object btnDataPump: TButton
    Left = 336
    Top = 20
    Width = 81
    Height = 25
    Caption = 'Data Pump'
    TabOrder = 0
    OnClick = btnDataPumpClick
  end
end
