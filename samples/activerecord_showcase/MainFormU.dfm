object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMVCActiveRecord - ShowCase'
  ClientHeight = 698
  ClientWidth = 1094
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    1094
    698)
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
    Top = 283
    Width = 121
    Height = 33
    Caption = 'Queries'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object Memo1: TMemo
    Left = 280
    Top = 8
    Width = 806
    Height = 682
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
    Top = 322
    Width = 121
    Height = 35
    Caption = 'Relations'
    TabOrder = 3
    OnClick = btnRelationsClick
  end
  object btnInheritance: TButton
    Left = 8
    Top = 363
    Width = 121
    Height = 34
    Caption = 'Inheritance'
    TabOrder = 4
    OnClick = btnInheritanceClick
  end
  object btnValidation: TButton
    Left = 8
    Top = 403
    Width = 121
    Height = 34
    Caption = 'Validation'
    TabOrder = 5
    OnClick = btnValidationClick
  end
  object btnMultiThreading: TButton
    Left = 144
    Top = 8
    Width = 121
    Height = 33
    Caption = 'Multi Threading'
    TabOrder = 6
    OnClick = btnMultiThreadingClick
  end
  object btnRQL: TButton
    Left = 8
    Top = 443
    Width = 121
    Height = 34
    Caption = 'RQL Query'
    TabOrder = 7
    OnClick = btnRQLClick
  end
  object btnReadOnlyFields: TButton
    Left = 8
    Top = 203
    Width = 121
    Height = 33
    Caption = 'CRUD With R/O Field'
    TabOrder = 8
    OnClick = btnReadOnlyFieldsClick
  end
  object btnNullTest: TButton
    Left = 144
    Top = 47
    Width = 121
    Height = 33
    Caption = 'Nullables'
    TabOrder = 9
    OnClick = btnNullTestClick
  end
  object btnCRUDNoAutoInc: TButton
    Left = 8
    Top = 86
    Width = 121
    Height = 33
    Caption = 'CRUD (no autoinc)'
    TabOrder = 10
    OnClick = btnCRUDNoAutoIncClick
  end
  object btnCRUDWithStringPKs: TButton
    Left = 8
    Top = 125
    Width = 121
    Height = 33
    Caption = 'CRUD (string pks)'
    TabOrder = 11
    OnClick = btnCRUDWithStringPKsClick
  end
  object btnWithSpaces: TButton
    Left = 8
    Top = 164
    Width = 121
    Height = 33
    Caption = 'CRUD (entity with spaces)'
    TabOrder = 12
    WordWrap = True
    OnClick = btnWithSpacesClick
  end
  object btnCountWithRQL: TButton
    Left = 144
    Top = 86
    Width = 121
    Height = 33
    Caption = 'Count with RQL'
    TabOrder = 13
    OnClick = btnCountWithRQLClick
  end
  object btnReadAndWriteOnly: TButton
    Left = 144
    Top = 125
    Width = 121
    Height = 33
    Caption = 'R/O, R/W'
    TabOrder = 14
    OnClick = btnReadAndWriteOnlyClick
  end
  object btnClientGeneratedPK: TButton
    Left = 144
    Top = 164
    Width = 121
    Height = 33
    Caption = 'Client Generated PKs'
    TabOrder = 15
    OnClick = btnClientGeneratedPKClick
  end
  object btnAttributes: TButton
    Left = 144
    Top = 203
    Width = 121
    Height = 33
    Caption = 'Attributes'
    TabOrder = 16
    OnClick = btnAttributesClick
  end
  object btnJSON_XML_Types: TButton
    Left = 144
    Top = 242
    Width = 121
    Height = 35
    Caption = 'JSON && XML'
    TabOrder = 17
    OnClick = btnJSON_XML_TypesClick
  end
  object btnMerge: TButton
    Left = 144
    Top = 283
    Width = 121
    Height = 34
    Caption = 'Merge'
    TabOrder = 18
    OnClick = btnMergeClick
  end
  object btnTableFilter: TButton
    Left = 144
    Top = 323
    Width = 121
    Height = 34
    Caption = 'Table Filter'
    TabOrder = 19
    OnClick = btnTableFilterClick
  end
  object btnPartitioning: TButton
    Left = 144
    Top = 363
    Width = 121
    Height = 33
    Caption = 'Table Partitioning'
    TabOrder = 20
    OnClick = btnPartitioningClick
  end
  object btnCRUDWithGUID: TButton
    Left = 8
    Top = 47
    Width = 121
    Height = 33
    Caption = 'CRUD (with GUID PK)'
    TabOrder = 21
    OnClick = btnCRUDWithGUIDClick
  end
  object btnOOP: TButton
    Left = 144
    Top = 402
    Width = 121
    Height = 34
    Caption = 'OOP with Partitioning and Filtering'
    TabOrder = 22
    WordWrap = True
    OnClick = btnOOPClick
  end
  object btnReadOnly: TButton
    Left = 8
    Top = 483
    Width = 121
    Height = 34
    Caption = 'Read/Only Entities'
    TabOrder = 23
    OnClick = btnReadOnlyClick
  end
  object btnSpeed: TButton
    Left = 8
    Top = 523
    Width = 121
    Height = 34
    Caption = 'Metadata Speed Test'
    TabOrder = 24
    OnClick = btnSpeedClick
  end
  object btnRefresh: TButton
    Left = 144
    Top = 442
    Width = 121
    Height = 34
    Caption = 'Manual Refresh'
    TabOrder = 25
    OnClick = btnRefreshClick
  end
  object btnNamedQuery: TButton
    Left = 144
    Top = 482
    Width = 121
    Height = 34
    Caption = 'Named Query'
    TabOrder = 26
    OnClick = btnNamedQueryClick
  end
  object btnVirtualEntities: TButton
    Left = 144
    Top = 522
    Width = 121
    Height = 34
    Caption = 'Virtual Entities'
    TabOrder = 27
    OnClick = btnVirtualEntitiesClick
  end
  object btnIntegersAsBool: TButton
    Left = 8
    Top = 563
    Width = 121
    Height = 34
    Caption = 'Integers As Booleans'
    TabOrder = 28
    OnClick = btnIntegersAsBoolClick
  end
  object btnObjectVersion: TButton
    Left = 8
    Top = 603
    Width = 121
    Height = 34
    Caption = 'Object Version'
    TabOrder = 29
    OnClick = btnObjectVersionClick
  end
  object btnCustomTable: TButton
    Left = 144
    Top = 562
    Width = 121
    Height = 34
    Caption = 'Custom TableName'
    TabOrder = 30
    OnClick = btnCustomTableClick
  end
  object btnCRUDWithOptions: TButton
    Left = 8
    Top = 242
    Width = 121
    Height = 33
    Caption = 'CRUD With Fields Opts'
    TabOrder = 31
    OnClick = btnCRUDWithOptionsClick
  end
  object btnTransaction: TButton
    Left = 144
    Top = 602
    Width = 121
    Height = 35
    Caption = 'TransactionContext'
    TabOrder = 32
    OnClick = btnTransactionClick
  end
  object btnUseExplicitConnection: TButton
    Left = 8
    Top = 643
    Width = 121
    Height = 34
    Caption = 'Use Explicit Connection'
    TabOrder = 33
    WordWrap = True
    OnClick = btnUseExplicitConnectionClick
  end
  object btnErrorWith2PKs: TButton
    Left = 144
    Top = 643
    Width = 121
    Height = 34
    Caption = 'Raise Error if More Thank One PK'
    TabOrder = 34
    WordWrap = True
    OnClick = btnErrorWith2PKsClick
  end
end
