object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LoggerGUI'
  ClientHeight = 146
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnLoggerTest: TButton
    Left = 8
    Top = 8
    Width = 217
    Height = 73
    Caption = 'Click me to get some logs'
    TabOrder = 0
    OnClick = btnLoggerTestClick
  end
end
