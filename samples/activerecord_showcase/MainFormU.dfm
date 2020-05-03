object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMVCActiveRecord - ShowCase'
  ClientHeight = 522
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    635
    522)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCRUD: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 33
    Caption = 'CRUD'
    TabOrder = 0
    OnClick = btnCRUDClick
  end
  object btnSelect: TButton
    Left = 8
    Top = 203
    Width = 121
    Height = 33
    Caption = 'Queries'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object Memo1: TMemo
    Left = 135
    Top = 8
    Width = 492
    Height = 506
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WantReturns = False
    WordWrap = False
  end
  object btnRelations: TButton
    Left = 8
    Top = 242
    Width = 121
    Height = 35
    Caption = 'Relations'
    TabOrder = 3
    OnClick = btnRelationsClick
  end
  object btnInheritance: TButton
    Left = 8
    Top = 283
    Width = 121
    Height = 34
    Caption = 'Inheritance'
    TabOrder = 4
    OnClick = btnInheritanceClick
  end
  object btnValidation: TButton
    Left = 8
    Top = 323
    Width = 121
    Height = 34
    Caption = 'Validation'
    TabOrder = 5
    OnClick = btnValidationClick
  end
  object btnMultiThreading: TButton
    Left = 8
    Top = 403
    Width = 121
    Height = 34
    Caption = 'Multi Threading'
    TabOrder = 6
    OnClick = btnMultiThreadingClick
  end
  object btnRQL: TButton
    Left = 8
    Top = 363
    Width = 121
    Height = 34
    Caption = 'RQL Query'
    TabOrder = 7
    OnClick = btnRQLClick
  end
  object btnTransientFields: TButton
    Left = 8
    Top = 164
    Width = 121
    Height = 33
    Caption = 'CRUD Transient'
    TabOrder = 8
    OnClick = btnTransientFieldsClick
  end
  object btnNullTest: TButton
    Left = 8
    Top = 443
    Width = 121
    Height = 34
    Caption = 'Nullables'
    TabOrder = 9
    OnClick = btnNullTestClick
  end
  object btnCRUDNoAutoInc: TButton
    Left = 8
    Top = 47
    Width = 121
    Height = 33
    Caption = 'CRUD (no autoinc)'
    TabOrder = 10
    OnClick = btnCRUDNoAutoIncClick
  end
  object btnCRUDWithStringPKs: TButton
    Left = 8
    Top = 86
    Width = 121
    Height = 33
    Caption = 'CRUD (string pks)'
    TabOrder = 11
    OnClick = btnCRUDWithStringPKsClick
  end
  object btnWithSpaces: TButton
    Left = 8
    Top = 125
    Width = 121
    Height = 33
    Caption = 'CRUD (entity with spaces)'
    TabOrder = 12
    WordWrap = True
    OnClick = btnWithSpacesClick
  end
  object FDConnection1: TFDConnection
    Left = 176
    Top = 56
  end
end
