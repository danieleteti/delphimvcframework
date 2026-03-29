object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'SSEClient Sample'
  ClientHeight = 355
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object MessagesMemo: TMemo
    Left = 0
    Top = 0
    Width = 741
    Height = 355
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 737
    ExplicitHeight = 354
  end
end
