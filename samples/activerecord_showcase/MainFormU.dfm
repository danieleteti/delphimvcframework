object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMVCActiveRecord - ShowCase'
  ClientHeight = 1423
  ClientWidth = 2760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 240
  DesignSize = (
    2760
    1423)
  TextHeight = 34
  object btnCRUD: TButton
    Left = 20
    Top = 20
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD'
    TabOrder = 0
    OnClick = btnCRUDClick
  end
  object btnSelect: TButton
    Left = 20
    Top = 605
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Queries'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object Memo1: TMemo
    Left = 700
    Top = 20
    Width = 2040
    Height = 1383
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
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
    ExplicitWidth = 2020
    ExplicitHeight = 1381
  end
  object btnRelations: TButton
    Left = 20
    Top = 703
    Width = 303
    Height = 87
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Relations'
    TabOrder = 3
    OnClick = btnRelationsClick
  end
  object btnInheritance: TButton
    Left = 20
    Top = 805
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Inheritance'
    TabOrder = 4
    OnClick = btnInheritanceClick
  end
  object btnValidation: TButton
    Left = 20
    Top = 905
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Validation'
    TabOrder = 5
    OnClick = btnValidationClick
  end
  object btnMultiThreading: TButton
    Left = 360
    Top = 20
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Multi Threading'
    TabOrder = 6
    OnClick = btnMultiThreadingClick
  end
  object btnRQL: TButton
    Left = 20
    Top = 1005
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'RQL Query'
    TabOrder = 7
    OnClick = btnRQLClick
  end
  object btnReadOnlyFields: TButton
    Left = 20
    Top = 508
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD With R/O Field'
    TabOrder = 8
    OnClick = btnReadOnlyFieldsClick
  end
  object btnNullTest: TButton
    Left = 360
    Top = 118
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Nullables'
    TabOrder = 9
    OnClick = btnNullTestClick
  end
  object btnCRUDNoAutoInc: TButton
    Left = 20
    Top = 215
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD (no autoinc)'
    TabOrder = 10
    OnClick = btnCRUDNoAutoIncClick
  end
  object btnCRUDWithStringPKs: TButton
    Left = 20
    Top = 313
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD (string pks)'
    TabOrder = 11
    OnClick = btnCRUDWithStringPKsClick
  end
  object btnWithSpaces: TButton
    Left = 20
    Top = 410
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD (entity with spaces)'
    TabOrder = 12
    WordWrap = True
    OnClick = btnWithSpacesClick
  end
  object btnCountWithRQL: TButton
    Left = 360
    Top = 215
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Count with RQL'
    TabOrder = 13
    OnClick = btnCountWithRQLClick
  end
  object btnReadAndWriteOnly: TButton
    Left = 360
    Top = 313
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'R/O, R/W'
    TabOrder = 14
    OnClick = btnReadAndWriteOnlyClick
  end
  object btnClientGeneratedPK: TButton
    Left = 360
    Top = 410
    Width = 303
    Height = 83
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Client Generated PKs'
    TabOrder = 15
    OnClick = btnClientGeneratedPKClick
  end
  object btnAttributes: TButton
    Left = 360
    Top = 508
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Attributes'
    TabOrder = 16
    OnClick = btnAttributesClick
  end
  object btnJSON_XML_Types: TButton
    Left = 360
    Top = 605
    Width = 303
    Height = 88
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'JSON && XML'
    TabOrder = 17
    OnClick = btnJSON_XML_TypesClick
  end
  object btnMerge: TButton
    Left = 360
    Top = 708
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Merge'
    TabOrder = 18
    OnClick = btnMergeClick
  end
  object btnTableFilter: TButton
    Left = 360
    Top = 808
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Table Filter'
    TabOrder = 19
    OnClick = btnTableFilterClick
  end
  object btnPartitioning: TButton
    Left = 360
    Top = 908
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Table Partitioning'
    TabOrder = 20
    OnClick = btnPartitioningClick
  end
  object btnCRUDWithGUID: TButton
    Left = 20
    Top = 118
    Width = 303
    Height = 82
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'CRUD (with GUID PK)'
    TabOrder = 21
    OnClick = btnCRUDWithGUIDClick
  end
  object btnOOP: TButton
    Left = 360
    Top = 1005
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'OOP with Partitioning and Filtering'
    TabOrder = 22
    WordWrap = True
    OnClick = btnOOPClick
  end
  object btnReadOnly: TButton
    Left = 20
    Top = 1105
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Read/Only Entities'
    TabOrder = 23
    OnClick = btnReadOnlyClick
  end
  object btnSpeed: TButton
    Left = 20
    Top = 1205
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Metadata Speed Test'
    TabOrder = 24
    OnClick = btnSpeedClick
  end
  object btnRefresh: TButton
    Left = 360
    Top = 1105
    Width = 303
    Height = 85
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Manual Refresh'
    TabOrder = 25
    OnClick = btnRefreshClick
  end
  object FDConnection1: TFDConnection
    Left = 312
    Top = 40
  end
end
