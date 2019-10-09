object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MVCActiveRecord Entity Generator (alpha)'
  ClientHeight = 630
  ClientWidth = 863
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 169
    Width = 863
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 215
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 863
    Height = 41
    Align = alTop
    TabOrder = 0
    object cboConnectionDefs: TComboBox
      Left = 4
      Top = 14
      Width = 145
      Height = 21
      TabOrder = 0
      OnChange = cboConnectionDefsChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 863
    Height = 128
    Align = alTop
    Caption = 'Panel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 855
      Height = 13
      Align = alTop
      Caption = 'FireDAC connection parameters'
      ExplicitWidth = 152
    end
    object mmConnectionParams: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 23
      Width = 855
      Height = 101
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 172
    Width = 863
    Height = 458
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 2
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 861
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel4'
      ShowCaption = False
      TabOrder = 0
      object btnGenEntities: TButton
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 120
        Height = 35
        Align = alLeft
        Caption = 'Generate Entities'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnGenEntitiesClick
      end
      object btnGetTables: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 120
        Height = 35
        Align = alLeft
        Caption = 'Get Tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = btnGetTablesClick
      end
      object edtReplace: TEdit
        Left = 255
        Top = 10
        Width = 121
        Height = 21
        TabOrder = 2
      end
    end
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 4
      Top = 45
      Width = 855
      Height = 409
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Tables'
        object veTablesMapping: TValueListEditor
          Left = 0
          Top = 0
          Width = 847
          Height = 381
          Align = alClient
          KeyOptions = [keyDelete, keyUnique]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goEditing, goThumbTracking]
          TabOrder = 0
          TitleCaptions.Strings = (
            'Table'
            'ClassName')
          ColWidths = (
            254
            587)
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Generated Code'
        ImageIndex = 1
        object mmOutput: TMemo
          Left = 0
          Top = 41
          Width = 847
          Height = 340
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 847
          Height = 41
          Align = alTop
          Caption = 'Panel5'
          ShowCaption = False
          TabOrder = 1
          object btnSaveCode: TButton
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 75
            Height = 33
            Align = alLeft
            Caption = '&Save'
            TabOrder = 0
            OnClick = btnSaveCodeClick
          end
        end
      end
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=MSSQL')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 256
    Top = 56
  end
  object qry: TFDQuery
    Connection = FDConnection1
    FetchOptions.AssignedValues = [evRecsMax, evRowsetSize, evUnidirectional, evAutoFetchAll]
    FetchOptions.Unidirectional = True
    FetchOptions.RowsetSize = 1
    FetchOptions.RecsMax = 1
    FetchOptions.AutoFetchAll = afDisable
    Left = 328
    Top = 200
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 368
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 424
    Top = 104
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 448
    Top = 200
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Delphi Unit'
        FileMask = '*.pas'
      end>
    Options = []
    Left = 424
    Top = 320
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 432
    Top = 328
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 496
    Top = 384
  end
  object FDPhysFBDriverLink2: TFDPhysFBDriverLink
    Left = 400
    Top = 408
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 304
    Top = 352
  end
  object FDPhysMySQLDriverLink2: TFDPhysMySQLDriverLink
    Left = 616
    Top = 360
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 608
    Top = 280
  end
end
