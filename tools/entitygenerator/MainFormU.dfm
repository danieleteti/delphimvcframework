object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = '[DMVCFramework] MVCActiveRecord Entity Generator'
  ClientHeight = 687
  ClientWidth = 1129
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 21
  object Splitter1: TSplitter
    Left = 0
    Top = 542
    Width = 1129
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 543
    ExplicitWidth = 1196
  end
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 1129
    Height = 542
    ActivePage = tsConnectionDefinition
    Align = alClient
    TabOrder = 0
    object tsConnectionDefinition: TTabSheet
      Caption = 'Connection Definition'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 89
        Width = 1121
        Height = 417
        Align = alClient
        Caption = 'Panel1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 1113
          Height = 13
          Align = alTop
          Caption = 'FireDAC connection parameters'
          ExplicitWidth = 152
        end
        object mmConnectionParams: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 23
          Width = 715
          Height = 390
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          OnChange = mmConnectionParamsChange
        end
        object Panel6: TPanel
          Left = 722
          Top = 20
          Width = 398
          Height = 396
          Align = alRight
          BevelOuter = bvNone
          Caption = 'Panel6'
          ShowCaption = False
          TabOrder = 1
          object GroupBox1: TGroupBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 392
            Height = 390
            Align = alClient
            Caption = 'Filter by Schema'
            Padding.Left = 5
            Padding.Top = 5
            Padding.Right = 5
            Padding.Bottom = 5
            TabOrder = 0
            object lstSchema: TListBox
              AlignWithMargins = True
              Left = 10
              Top = 23
              Width = 372
              Height = 357
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
          end
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1121
        Height = 89
        Align = alTop
        TabOrder = 1
        object Label1: TLabel
          AlignWithMargins = True
          Left = 19
          Top = 12
          Width = 276
          Height = 21
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Caption = 'Select a FireDAC Connection Definitions'
          Layout = tlCenter
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 557
          Top = 11
          Width = 553
          Height = 67
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alRight
          Caption = 
            'Please, select the FireDAC connection definition from the combo ' +
            'box on the left. Then, if available, select the catalog and the ' +
            'schema where the tables are. In the next steps entities will be ' +
            'generated from that set of tables.'
          Layout = tlCenter
          WordWrap = True
          ExplicitHeight = 63
        end
        object cboConnectionDefs: TComboBox
          AlignWithMargins = True
          Left = 19
          Top = 46
          Width = 276
          Height = 29
          Style = csDropDownList
          TabOrder = 0
          OnChange = cboConnectionDefsChange
        end
      end
    end
    object tsTablesMapping: TTabSheet
      Caption = 'Tables Mapping'
      ImageIndex = 2
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 1121
        Height = 506
        Align = alClient
        Caption = 'Panel3'
        ShowCaption = False
        TabOrder = 0
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 1119
          Height = 151
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Panel4'
          ShowCaption = False
          TabOrder = 0
          DesignSize = (
            1119
            151)
          object btnGenEntities: TButton
            AlignWithMargins = True
            Left = 1147
            Top = 0
            Width = 161
            Height = 35
            Anchors = [akRight, akBottom]
            Caption = 'Generate Entities'
            TabOrder = 0
          end
          object chkGenerateMapping: TCheckBox
            AlignWithMargins = True
            Left = 10
            Top = 116
            Width = 1106
            Height = 32
            Margins.Left = 10
            Align = alBottom
            Caption = 
              'Register entities in ActiveRecordMappingRegistry (needed by TMVC' +
              'ActiveRecordController)'
            Checked = True
            State = cbChecked
            TabOrder = 1
            WordWrap = True
          end
          object rgNameCase: TRadioGroup
            Left = 7
            Top = 9
            Width = 354
            Height = 104
            Caption = 'Class MVCNameCase'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'LowerCase'
              'UpperCase'
              'CamelCase'
              'PascalCase'
              'SnakeCase'
              'AsIs')
            TabOrder = 2
          end
          object rgFieldNameFormatting: TRadioGroup
            Left = 367
            Top = 10
            Width = 355
            Height = 104
            Caption = 'Property Names Formatting'
            ItemIndex = 1
            Items.Strings = (
              'Leave names as is in database table'
              'Convert names to Pascal Case (eg FirstName)')
            TabOrder = 3
          end
          object gbOptions: TGroupBox
            Left = 728
            Top = 10
            Width = 384
            Height = 105
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Other Options'
            TabOrder = 4
            DesignSize = (
              384
              105)
            object Label5: TLabel
              Left = 31
              Top = 29
              Width = 324
              Height = 42
              Anchors = [akLeft, akTop, akRight]
              Caption = 
                'Declare classes as abstract (MVCTable must be redeclared on desc' +
                'endant classes)'
              WordWrap = True
            end
            object chkClassAsAbstract: TCheckBox
              Left = 9
              Top = 32
              Width = 20
              Height = 17
              TabOrder = 0
            end
          end
        end
        object PageControl1: TPageControl
          AlignWithMargins = True
          Left = 4
          Top = 206
          Width = 1113
          Height = 296
          ActivePage = TabSheet1
          Align = alClient
          TabOrder = 1
          object TabSheet1: TTabSheet
            Caption = 'Tables'
            object DBGrid1: TDBGrid
              Left = 0
              Top = 0
              Width = 1105
              Height = 219
              Align = alClient
              DataSource = dsrcTablesMapping
              DefaultDrawing = False
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -16
              TitleFont.Name = 'Segoe UI'
              TitleFont.Style = []
              OnCellClick = DBGrid1CellClick
              OnDrawColumnCell = DBGrid1DrawColumnCell
              Columns = <
                item
                  ButtonStyle = cbsNone
                  Expanded = False
                  FieldName = 'GENERATE'
                  PickList.Strings = (
                    'yes'
                    'no')
                  ReadOnly = True
                  Width = 86
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'TABLE_NAME'
                  ReadOnly = True
                  Width = 478
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'CLASS_NAME'
                  Visible = True
                end>
            end
            object Panel7: TPanel
              Left = 0
              Top = 219
              Width = 1105
              Height = 41
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 1
              object Label4: TLabel
                Left = 3
                Top = 9
                Width = 140
                Height = 21
                Caption = 'Filter for table name'
              end
              object EditTableNameFilter: TEdit
                Left = 149
                Top = 6
                Width = 276
                Height = 29
                TabOrder = 0
                OnChange = EditTableNameFilterChange
              end
            end
          end
        end
        object Panel10: TPanel
          Left = 1
          Top = 152
          Width = 1119
          Height = 51
          Align = alTop
          Caption = 'Panel10'
          ShowCaption = False
          TabOrder = 2
          object SpeedButton1: TSpeedButton
            AlignWithMargins = True
            Left = 180
            Top = 8
            Width = 162
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Select All'
            OnClick = SpeedButton1Click
          end
          object SpeedButton2: TSpeedButton
            AlignWithMargins = True
            Left = 352
            Top = 8
            Width = 163
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Select None'
            OnClick = SpeedButton2Click
          end
          object SpeedButton3: TSpeedButton
            AlignWithMargins = True
            Left = 525
            Top = 8
            Width = 162
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Invert Selection'
            OnClick = SpeedButton3Click
          end
          object btnGetTables: TButton
            AlignWithMargins = True
            Left = 7
            Top = 8
            Width = 163
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actRefreshTableList
            Constraints.MinWidth = 160
            TabOrder = 0
          end
        end
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 632
    Width = 1129
    Height = 55
    Margins.Right = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnPrev: TButton
      AlignWithMargins = True
      Left = 912
      Top = 3
      Width = 104
      Height = 49
      Action = TabPreviousTab1
      Align = alRight
      TabOrder = 0
    end
    object btnNext: TButton
      AlignWithMargins = True
      Left = 1022
      Top = 3
      Width = 104
      Height = 49
      Action = TabNextTab1
      Align = alRight
      TabOrder = 1
    end
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 894
      Height = 49
      Align = alLeft
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = 'Panel5'
      ShowCaption = False
      TabOrder = 2
      object Label6: TLabel
        Left = 7
        Top = 11
        Width = 126
        Height = 21
        Caption = 'Output File Name:'
      end
      object btnSaveAs: TSpeedButton
        Left = 654
        Top = 4
        Width = 55
        Height = 37
        Action = actSaveGeneratedCode
      end
      object EditOutputFileName: TEdit
        Left = 139
        Top = 8
        Width = 509
        Height = 29
        TabOrder = 0
      end
      object Button6: TButton
        AlignWithMargins = True
        Left = 715
        Top = 4
        Width = 159
        Height = 37
        Action = actGenerateCode
        Images = ImageListButtons
        TabOrder = 1
      end
    end
  end
  object Panel12: TPanel
    Left = 0
    Top = 545
    Width = 1129
    Height = 87
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel12'
    TabOrder = 2
    object lbLog: TListBox
      Left = 0
      Top = 0
      Width = 1129
      Height = 87
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      ItemHeight = 19
      ParentFont = False
      ScrollWidth = 5000
      TabOrder = 0
    end
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=MSSQL')
    ResourceOptions.AssignedValues = [rvKeepConnection]
    ResourceOptions.KeepConnection = False
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 408
    Top = 320
  end
  object qry: TFDQuery
    Connection = FDConnection
    FetchOptions.AssignedValues = [evRecsMax, evRowsetSize, evUnidirectional, evAutoFetchAll]
    FetchOptions.Unidirectional = True
    FetchOptions.RowsetSize = 1
    FetchOptions.RecsMax = 1
    FetchOptions.AutoFetchAll = afDisable
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    Left = 384
    Top = 232
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 504
    Top = 496
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 496
    Top = 280
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 784
    Top = 568
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Delphi Unit'
        FileMask = '*.pas'
      end>
    Options = []
    Left = 328
    Top = 416
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 616
    Top = 568
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 784
    Top = 496
  end
  object FDPhysFBDriverLink2: TFDPhysFBDriverLink
    Left = 632
    Top = 408
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 632
    Top = 496
  end
  object FDPhysMySQLDriverLink2: TFDPhysMySQLDriverLink
    Left = 524
    Top = 708
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 504
    Top = 408
  end
  object dsTablesMapping: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'TABLE_NAME'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'CLASS_NAME'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'GENERATE'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 72
    Top = 432
    object dsTablesMappingGENERATE: TBooleanField
      DisplayLabel = 'Generate?'
      FieldName = 'GENERATE'
    end
    object dsTablesMappingTABLE_NAME: TStringField
      DisplayLabel = 'Table Name'
      DisplayWidth = 60
      FieldName = 'TABLE_NAME'
      Size = 100
    end
    object dsTablesMappingCLASS_NAME: TStringField
      DisplayLabel = 'Class Name'
      DisplayWidth = 60
      FieldName = 'CLASS_NAME'
      Size = 100
    end
  end
  object dsrcTablesMapping: TDataSource
    DataSet = dsTablesMapping
    Left = 184
    Top = 424
  end
  object ProjectFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileName = 'C:\DEV\dmvcframework\tools\entitygenerator'
    FileTypes = <
      item
        DisplayName = 'DMVC Entities Generator Project'
        FileMask = '*.entgen'
      end>
    Options = []
    Left = 280
    Top = 256
  end
  object MainMenu1: TMainMenu
    Images = ImageListMainMenu
    Left = 728
    Top = 168
    object File1: TMenuItem
      Caption = '&File'
      object NewProject1: TMenuItem
        Action = actNewProject
      end
      object LoadProject1: TMenuItem
        Action = actLoadProject
      end
      object SaveProject1: TMenuItem
        Action = actSaveProject
      end
      object Saveprojectas1: TMenuItem
        Action = actSaveProjectAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = FileExit1
      end
    end
    object Entities1: TMenuItem
      Caption = '&Entities'
      object RefreshCatalog1: TMenuItem
        Caption = 'Retrieve metadata'
        ImageIndex = 6
        ShortCut = 8308
      end
      object RefreshTableList1: TMenuItem
        Action = actRefreshTableList
      end
      object GenerateCode1: TMenuItem
        Action = actGenerateCode
      end
      object SaveGeneratedCode1: TMenuItem
        Action = actSaveGeneratedCode
      end
    end
  end
  object ActionList1: TActionList
    Images = ImageListMainMenu
    Left = 844
    Top = 180
    object actNewProject: TAction
      Caption = 'New Project'
      ImageIndex = 0
      OnExecute = actNewProjectExecute
    end
    object actLoadProject: TAction
      Caption = 'Load Project'
      ImageIndex = 1
      OnExecute = actLoadProjectExecute
    end
    object actSaveProject: TAction
      Caption = 'Save Project'
      ImageIndex = 2
      OnExecute = actSaveProjectExecute
    end
    object actSaveProjectAs: TAction
      ImageIndex = 3
      OnExecute = actSaveProjectAsExecute
    end
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object TabNextTab1: TNextTab
      Category = 'Tab'
      TabControl = pcMain
      Caption = '&Next'
      Enabled = False
      Hint = 'Next|Go to the next tab'
      AfterTabChange = TabNextTab1AfterTabChange
      OnUpdate = TabNextTab1Update
    end
    object TabPreviousTab1: TPreviousTab
      Category = 'Tab'
      TabControl = pcMain
      Caption = '&Previous'
      Enabled = False
      Hint = 'Previous|Go back to the previous tab'
    end
    object actSaveGeneratedCode: TAction
      ImageIndex = 4
      ShortCut = 16467
      OnExecute = actSaveGeneratedCodeExecute
    end
    object actGenerateCode: TAction
      Caption = 'Generate Code'
      ImageIndex = 5
      ShortCut = 120
      OnExecute = actGenerateCodeExecute
      OnUpdate = actGenerateCodeUpdate
    end
    object actRefreshTableList: TAction
      Caption = 'Refresh Table List'
      ImageIndex = 6
      ShortCut = 116
      OnExecute = actRefreshTableListExecute
      OnUpdate = actRefreshTableListUpdate
    end
  end
  object FileSaveDialogProject: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'DMVC Entities Generator'
        FileMask = '*.entgen'
      end>
    Options = []
    Left = 328
    Top = 360
  end
  object ImageListMainMenu: TImageList
    ColorDepth = cdDefault
    Height = 32
    Width = 32
    Left = 848
    Top = 324
    Bitmap = {
      494C010107000800040020002000FFFFFFFF0510FFFFFFFFFFFFFFFF424D7600
      0000000000007600000028000000800000004000000001000400000000000010
      0000000000000000000000000000000000000000000000008000008000000080
      800080000000800080008080000080808000C0C0C0000000FF0000FF000000FF
      FF00FF000000FF00FF00FFFF0000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000400000000100010000000000000400000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFF09FFFFFFFF00FFF00000000
      FFFF3FFFE0CFFFFFFF8001FF00000000FFFE1FFFC7C3FFFFFE0FF07F00000000
      FFFC0FFF9FC0FFFFFC3FFC3F00000000FFF807FF1FCC7FFFF8FFFF1F00000000
      FFF123FF3FCE7FFFF1FFFF8F00000000FFFB37FF3FCF3FFFE3FFFFC700000000
      FFFF3FFF3FCF3FFFC7FFFFE300000000FFFF3FFF3FE79FFFCFFFFFF300000000
      FFFF3FFF1FE79FFF8FFFFFF100000000FFFFFFFF9FF3CFFF9FFFFFF900000000
      FFFFFFFFC7F3CEFF9FFFFFF900000000FF00003FE039E7033FFFFFFC00000000
      FE00003FF019E7013FFFFFFC00000000FC4CF33FFFFCF3F83FFFFFFC00000000
      FCCCF33FFFFCF3FC3FFFFFFC00000000FCCFF33FFFFE79FC3FFFFFFC00000000
      FCCFF33FFFFE79F83FFFFFFC00000000FCC0033FFFF33CE13FFFFFFC00000000
      FCC0033FFFF93CE33FFFFFFC00000000FCFFFF3FFFF99E7F9FFFFFF900000000
      FCFFFF3FFFFC9E7F9FFFFFF900000000FCC0033FFFFCCF3F8FFFFFF100000000
      FCC0033FFFFE4F3FCFFFFFF300000000FCCFF33FFFFE679FC7CFFFE300000000
      FCCFF33FFFFF279FE3CFFFC700000000FCCFF33FFFFF038FF1CFFF8F00000000
      FCCFF33FFFFF921FF8CFFF1F00000000FCCFF33FFFFFF84FFC0FFC3F00000000
      FCCFF33FFFFFF8CFFE0FF07F00000000FC00003FFFFFFE0FF00FF1FF00000000
      FE00007FFFFFFE3FF00FFFFF00000000F00000FFFFFFFFFFFFFFFFFFFFFCFFFF
      F000007FFFFFFFFFFFFF3FFFFFFC0FFFF3FFFE3FFFFFFFFFFFFE1FFFFFFE07FF
      F3FFFF1FFFFFFFFFFFFC0FFFFFFE63FFF3FFFF8F0000007FFFF807FFFFFE71FF
      F3FFFFCF0000003FFFF123FFFFFE38FFF3FFFFCF1FFFFF3FFFFB37FFF0071C7F
      F3FFFFCF1FFFFF9FFFFF3FFFE0038E3FF3FFFFCF0FFFFF9FFFFF3FFFC4CFC71F
      F3FFFFCF0FFFFFCFFFFF3FFFCCCFE38FF3FFFFCF27FFFFCFFFFFFFFFCCCFF1C7
      F3FFFFCF27FFFFE7FFFFFFFFCCCFF8E3F3FFFFCF33FFFFE7FF00003FCCFFDC71
      F3FFFFCF33FFFFF3FE00003FCCFFCE38F3FFFFCF39FFFFF3FC4CF33FCC000F1C
      F3FFFFCF39FFFFF9FCCCF33FCC000F8CF3FFFFCF3CFFFFF9FCCFF33FCFFFFFC0
      F3FFFFCF3CFFFFFCFCCFF33FCFFFFFE1F3FFFFCF3E000000FCC0033FCC00037F
      F3FFFFCF3E000001FCC0033FCC00033FF3FFFFCF3FFFFF3FFCFFFF3FCCFFF33F
      F3FFFFCF3FFFFF3FFCFFFF3FCCFFF33FF3FFFFCF3FFFFF3FFCC0033FCCFFF33F
      F3FFFFCF3FFFFF3FFCC0033FCCFFF33FF3FFFFCF3FC0003FFCCFF33FCCFFF33F
      F3FFFFCF3F80007FFCCFF33FCCFFF33FF3FFFFCF001FFFFFFCCFF33FCCFFF33F
      F1FFFFCF803FFFFFFCCFF33FCCFFF33FF8FFFFCFFFFFFFFFFCCFF33FC000003F
      FC7FFFCFFFFFFFFFFCCFF33FE000007FFE00000FFFFFFFFFFC00003FFFFFFFFF
      FF00000FFFFFFFFFFE00007FFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object ImageListButtons: TImageList
    ColorDepth = cdDefault
    Height = 24
    Width = 24
    Left = 2260
    Top = 400
    Bitmap = {
      494C010101000800040018001800FFFFFFFF0510FFFFFFFFFFFFFFFF424D7600
      0000000000007600000028000000600000001800000001000400000000008004
      0000000000000000000000000000000000000000000000008000008000000080
      800080000000800080008080000080808000C0C0C0000000FF0000FF000000FF
      FF00FF000000FF00FF00FFFF0000FFFFFF000000000009000090000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000999990099999000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000099000900900099000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000900099990009000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009000000000090000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000009000900000000009000900000000000000000000000000000000000000
      0000000000000000000000000000000000000999999000000000099999900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000090000000000000000000090000000000000000000000000000000000000
      0000000000000000000000000000000000000900000009999990000000900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000990000009900009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000999000090000009000099900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000900009000000900009000000000000000000000000000000000000000
      0000000000000000000000000000000000000009000090000009000090000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000099900009000000900009990000000000000000000000000000000000000
      0000000000000000000000000000000000009900000099000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000090000000999999000000090000000000000000000000000000000000000
      0000000000000000000000000000000000000900000000000000000000900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000099999900000000009999990000000000000000000000000000000000000
      0000000000000000000000000000000000000090009000000000090009000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000900000000009000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009000999900090000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000009900090090009900000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009999900999990000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000900009000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFBDFF000000000000000000FC183F00
      0000000000000000F9DB9F000000000000000000FDC3BF000000000000000000
      FDFFBF000000000000000000DDFFBB00000000000000000081FF810000000000
      00000000BFFFFD000000000000000000BF81FD0000000000000000003F3CFC00
      00000000000000008F7EF1000000000000000000EF7EF7000000000000000000
      EF7EF70000000000000000008F7EF10000000000000000003F3CFC0000000000
      00000000BF81FD000000000000000000BFFFFD00000000000000000081FF8100
      0000000000000000DDFFBB000000000000000000FDFFBF000000000000000000
      FDC3BF000000000000000000F9DB9F000000000000000000FC183F0000000000
      00000000FFBDFF00000000000000000000000000000000000000000000000000
      000000000000}
  end
  object qryMeta: TFDMetaInfoQuery
    Connection = FDConnection
    MetaInfoKind = mkTableFields
    Left = 96
    Top = 176
  end
  object FDMoniFlatFileClientLink1: TFDMoniFlatFileClientLink
    Left = 1080
    Top = 200
  end
  object FDMoniCustomClientLink1: TFDMoniCustomClientLink
    Left = 1080
    Top = 256
  end
  object FDMoniRemoteClientLink1: TFDMoniRemoteClientLink
    Left = 1080
    Top = 312
  end
end
