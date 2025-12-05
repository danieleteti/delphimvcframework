object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'dotEnv :: Demo'
  ClientHeight = 353
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    843
    353)
  TextHeight = 32
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 827
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
end
