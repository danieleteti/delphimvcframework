object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'ObjectPool Sample'
  ClientHeight = 583
  ClientWidth = 1254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1254
    583)
  TextHeight = 21
  object Label1: TLabel
    Left = 8
    Top = 160
    Width = 225
    Height = 294
    Caption = 
      'Click 2 times "No Pool" (note spent time). Now click 2 times on ' +
      '"With Pool" and be amazed!'#13#10#13#10'In this demo the "Pooled Object" s' +
      'pends rougly 1ms to be initialized - the more the initialization' +
      ' takes, more the pool is effective.'#13#10#13#10'Any strange delay in "Wit' +
      'h Pool" is caused by the "shrinker thread" which tries to reduce' +
      ' the pool size every X seconds.'
    WordWrap = True
  end
  object btnUnlimitedPool: TButton
    Left = 8
    Top = 79
    Width = 226
    Height = 57
    Caption = 'With Pool'
    TabOrder = 0
    OnClick = btnUnlimitedPoolClick
  end
  object lbLog: TListBox
    Left = 240
    Top = 16
    Width = 1006
    Height = 559
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    ItemHeight = 19
    ParentFont = False
    TabOrder = 1
  end
  object btnPlain: TButton
    Left = 8
    Top = 16
    Width = 226
    Height = 57
    Caption = 'No Pool'
    TabOrder = 2
    OnClick = btnPlainClick
  end
  object btnPoolSize: TButton
    Left = 8
    Top = 479
    Width = 113
    Height = 25
    Caption = 'Get Pool Size'
    TabOrder = 3
    OnClick = btnPoolSizeClick
  end
end
