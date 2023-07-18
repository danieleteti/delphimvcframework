object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'dotEnv :: ShowCase'
  ClientHeight = 442
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    824
    442)
  TextHeight = 15
  object Shape1: TShape
    Left = 8
    Top = 181
    Width = 169
    Height = 4
    Brush.Color = clGray
  end
  object btnSimple: TButton
    Left = 8
    Top = 16
    Width = 169
    Height = 49
    Caption = 'default ENV'
    TabOrder = 0
    OnClick = btnSimpleClick
  end
  object mmVars: TMemo
    Left = 183
    Top = 16
    Width = 633
    Height = 418
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 629
    ExplicitHeight = 417
  end
  object btnTestEnv: TButton
    Left = 8
    Top = 71
    Width = 169
    Height = 49
    Caption = 'Test ENV (default + test)'
    TabOrder = 2
    OnClick = btnTestEnvClick
  end
  object btnProdEnv: TButton
    Left = 8
    Top = 126
    Width = 169
    Height = 49
    Caption = 'Prod ENV (default + prod)'
    TabOrder = 3
    OnClick = btnProdEnvClick
  end
  object btnSingleEnv: TButton
    Left = 8
    Top = 191
    Width = 169
    Height = 49
    Caption = 'Single ENV without inheritance (only prod)'
    TabOrder = 4
    WordWrap = True
    OnClick = btnSingleEnvClick
  end
end
