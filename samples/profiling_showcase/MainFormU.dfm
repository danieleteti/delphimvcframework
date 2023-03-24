object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Profiler :: Sample'
  ClientHeight = 328
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnSimple: TButton
    Left = 16
    Top = 16
    Width = 129
    Height = 41
    Caption = 'Simple Profiling'
    TabOrder = 0
    OnClick = btnSimpleClick
  end
  object btnNestedCalls: TButton
    Left = 16
    Top = 80
    Width = 129
    Height = 41
    Caption = 'Nested Calls'
    TabOrder = 1
    OnClick = btnNestedCallsClick
  end
  object btnNestedCallsInLoop: TButton
    Left = 16
    Top = 144
    Width = 129
    Height = 41
    Caption = 'Nested Calls In Loop'
    TabOrder = 2
    OnClick = btnNestedCallsInLoopClick
  end
end
