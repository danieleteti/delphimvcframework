object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Profiler :: Sample'
  ClientHeight = 232
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    560
    232)
  TextHeight = 15
  object btnSimple: TButton
    Left = 16
    Top = 24
    Width = 129
    Height = 41
    Caption = 'Simple Profiling'
    TabOrder = 0
    OnClick = btnSimpleClick
  end
  object btnNestedCalls: TButton
    Left = 16
    Top = 96
    Width = 129
    Height = 41
    Caption = 'Nested Calls'
    TabOrder = 1
    OnClick = btnNestedCallsClick
  end
  object btnNestedCallsInLoop: TButton
    Left = 16
    Top = 168
    Width = 129
    Height = 41
    Caption = 'Nested Calls In Loop'
    TabOrder = 2
    OnClick = btnNestedCallsInLoopClick
  end
  object chkLogsThreshold: TCheckBox
    Left = 362
    Top = 108
    Width = 182
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Logs Only If Over Threshold'
    TabOrder = 3
    OnClick = chkLogsThresholdClick
  end
end
