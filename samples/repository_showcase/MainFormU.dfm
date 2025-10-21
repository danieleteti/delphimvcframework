object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DMVCFramework - Repository Pattern Showcase'
  ClientHeight = 561
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Memo1: TMemo
    Left = 186
    Top = 0
    Width = 698
    Height = 561
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 186
    Height = 561
    Align = alLeft
    TabOrder = 1
    object btnRunAllShowcases: TButton
      Left = 8
      Top = 256
      Width = 169
      Height = 41
      Caption = 'RUN ALL SHOWCASES'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btnRunAllShowcasesClick
    end
    object btnClearLog: TButton
      Left = 8
      Top = 303
      Width = 169
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 1
      OnClick = btnClearLogClick
    end
    object btnAdvancedRepository: TButton
      Left = 8
      Top = 203
      Width = 169
      Height = 33
      Caption = 'Advanced Repository'
      TabOrder = 2
      OnClick = btnAdvancedRepositoryClick
    end
    object btnBasicCRUD: TButton
      Left = 8
      Top = 8
      Width = 169
      Height = 33
      Caption = 'Basic CRUD Operations'
      TabOrder = 3
      OnClick = btnBasicCRUDClick
    end
    object btnCustomMethods: TButton
      Left = 8
      Top = 164
      Width = 169
      Height = 33
      Caption = 'Custom Repository Methods'
      TabOrder = 4
      OnClick = btnCustomMethodsClick
    end
    object btnNamedQueries: TButton
      Left = 8
      Top = 125
      Width = 169
      Height = 33
      Caption = 'Named Queries'
      TabOrder = 5
      OnClick = btnNamedQueriesClick
    end
    object btnQueryOperations: TButton
      Left = 8
      Top = 47
      Width = 169
      Height = 33
      Caption = 'Query Operations'
      TabOrder = 6
      OnClick = btnQueryOperationsClick
    end
    object btnRQLOperations: TButton
      Left = 8
      Top = 86
      Width = 169
      Height = 33
      Caption = 'RQL Operations'
      TabOrder = 7
      OnClick = btnRQLOperationsClick
    end
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 440
    Top = 288
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 448
    Top = 296
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 456
    Top = 304
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 464
    Top = 312
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 472
    Top = 320
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 480
    Top = 328
  end
end
