object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = '[DMVCFramework] MVCActiveRecord Entity Generator'
  ClientHeight = 688
  ClientWidth = 1189
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
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 1189
    Height = 644
    ActivePage = tsTablesMapping
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1199
    ExplicitHeight = 638
    object tsConnectionDefinition: TTabSheet
      Caption = 'Connection Definition'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 89
        Width = 1181
        Height = 519
        Align = alClient
        Caption = 'Panel1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 1191
        ExplicitHeight = 513
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 1173
          Height = 13
          Align = alTop
          Caption = 'FireDAC connection parameters'
          ExplicitWidth = 152
        end
        object mmConnectionParams: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 23
          Width = 775
          Height = 446
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
          ExplicitWidth = 785
          ExplicitHeight = 440
        end
        object Panel6: TPanel
          Left = 782
          Top = 20
          Width = 398
          Height = 452
          Align = alRight
          BevelOuter = bvNone
          Caption = 'Panel6'
          ShowCaption = False
          TabOrder = 1
          ExplicitLeft = 792
          ExplicitHeight = 446
          object GroupBox1: TGroupBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 392
            Height = 446
            Align = alClient
            Caption = 'Select a catalog and then the schema where your tables are'
            Padding.Left = 5
            Padding.Top = 5
            Padding.Right = 5
            Padding.Bottom = 5
            TabOrder = 0
            ExplicitHeight = 440
            object lstSchema: TListBox
              AlignWithMargins = True
              Left = 191
              Top = 50
              Width = 191
              Height = 386
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
              ExplicitHeight = 380
            end
            object lstCatalog: TListBox
              AlignWithMargins = True
              Left = 10
              Top = 50
              Width = 175
              Height = 386
              Align = alLeft
              ItemHeight = 13
              TabOrder = 1
              OnClick = lstCatalogClick
              ExplicitHeight = 380
            end
            object Panel11: TPanel
              Left = 7
              Top = 20
              Width = 378
              Height = 27
              Align = alTop
              BevelOuter = bvNone
              Caption = 'Panel11'
              ShowCaption = False
              TabOrder = 2
              object Label4: TLabel
                Left = 3
                Top = 11
                Width = 42
                Height = 13
                Caption = 'Catalogs'
              end
              object Label5: TLabel
                Left = 184
                Top = 11
                Width = 42
                Height = 13
                Caption = 'Schemas'
              end
            end
          end
        end
        object Panel9: TPanel
          Left = 1
          Top = 472
          Width = 1179
          Height = 46
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'Panel9'
          ShowCaption = False
          TabOrder = 2
          ExplicitTop = 466
          ExplicitWidth = 1189
          object btnRefreshCatalog: TButton
            AlignWithMargins = True
            Left = 1043
            Top = 3
            Width = 130
            Height = 40
            Margins.Right = 6
            Action = actRefreshCatalog
            Align = alRight
            ImageIndex = 0
            Images = ImageListButtons
            TabOrder = 0
            ExplicitLeft = 1053
          end
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1181
        Height = 89
        Align = alTop
        TabOrder = 1
        ExplicitWidth = 1191
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
          Left = 589
          Top = 11
          Width = 581
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
          ExplicitLeft = 599
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
        Width = 1181
        Height = 608
        Align = alClient
        Caption = 'Panel3'
        ShowCaption = False
        TabOrder = 0
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 1179
          Height = 208
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Panel4'
          ShowCaption = False
          TabOrder = 0
          ExplicitWidth = 1189
          DesignSize = (
            1179
            208)
          object btnGenEntities: TButton
            AlignWithMargins = True
            Left = 1232
            Top = 57
            Width = 161
            Height = 35
            Anchors = [akRight, akBottom]
            Caption = 'Generate Entities'
            TabOrder = 0
            ExplicitLeft = 1242
          end
          object chkGenerateMapping: TCheckBox
            AlignWithMargins = True
            Left = 10
            Top = 173
            Width = 1166
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
            ExplicitWidth = 1176
          end
          object rgNameCase: TRadioGroup
            Left = 7
            Top = 49
            Width = 393
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
            Left = 406
            Top = 49
            Width = 389
            Height = 104
            Caption = 'Field Names Formatting'
            ItemIndex = 1
            Items.Strings = (
              'Leave field names as in database table'
              'Format field names as Pascal Case (eg FirstName)')
            TabOrder = 3
          end
        end
        object PageControl1: TPageControl
          AlignWithMargins = True
          Left = 4
          Top = 311
          Width = 1173
          Height = 293
          ActivePage = TabSheet1
          Align = alClient
          TabOrder = 1
          ExplicitTop = 260
          ExplicitWidth = 1183
          ExplicitHeight = 338
          object TabSheet1: TTabSheet
            Caption = 'Tables'
            object DBGrid1: TDBGrid
              Left = 0
              Top = 41
              Width = 1165
              Height = 216
              Align = alClient
              BorderStyle = bsNone
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
              Top = 0
              Width = 1165
              Height = 41
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitWidth = 1175
              object btnUZ: TButton
                AlignWithMargins = True
                Left = 598
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'U..Z'
                TabOrder = 0
                OnClick = btnUZClick
              end
              object Button1: TButton
                AlignWithMargins = True
                Left = 360
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'L..Q'
                TabOrder = 1
                OnClick = Button1Click
              end
              object Button2: TButton
                AlignWithMargins = True
                Left = 241
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'E..K'
                TabOrder = 2
                OnClick = Button2Click
              end
              object Button3: TButton
                AlignWithMargins = True
                Left = 122
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'C..D'
                TabOrder = 3
                OnClick = Button3Click
              end
              object Button4: TButton
                AlignWithMargins = True
                Left = 479
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'R..T'
                TabOrder = 4
                OnClick = btnSlice1Click
              end
              object Button5: TButton
                AlignWithMargins = True
                Left = 3
                Top = 3
                Width = 113
                Height = 35
                Align = alLeft
                Caption = 'A..D'
                TabOrder = 5
                OnClick = Button5Click
              end
            end
          end
        end
        object Panel10: TPanel
          Left = 1
          Top = 209
          Width = 1179
          Height = 99
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
            Left = 7
            Top = 54
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
            Left = 180
            Top = 54
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
            Caption = 'Refresh Table List'
            Constraints.MinWidth = 160
            TabOrder = 0
          end
          object Panel5: TPanel
            AlignWithMargins = True
            Left = 486
            Top = 4
            Width = 689
            Height = 91
            Align = alRight
            BevelKind = bkTile
            BevelOuter = bvNone
            Caption = 'Panel5'
            ShowCaption = False
            TabOrder = 1
            ExplicitTop = 3
            object Label6: TLabel
              Left = 12
              Top = 12
              Width = 126
              Height = 21
              Caption = 'Output File Name:'
            end
            object btnSaveAs: TSpeedButton
              Left = 479
              Top = 37
              Width = 55
              Height = 37
              Action = actSaveGeneratedCode
            end
            object EditOutputFileName: TEdit
              Left = 12
              Top = 39
              Width = 461
              Height = 29
              TabOrder = 0
            end
            object Button6: TButton
              AlignWithMargins = True
              Left = 539
              Top = 37
              Width = 136
              Height = 37
              Action = actGenerateCode
              TabOrder = 1
            end
          end
        end
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 644
    Width = 1189
    Height = 44
    Margins.Right = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1199
    object btnPrev: TButton
      AlignWithMargins = True
      Left = 972
      Top = 3
      Width = 104
      Height = 38
      Action = TabPreviousTab1
      Align = alRight
      TabOrder = 0
      ExplicitLeft = 696
      ExplicitTop = 6
      ExplicitHeight = 44
    end
    object btnNext: TButton
      AlignWithMargins = True
      Left = 1082
      Top = 3
      Width = 104
      Height = 38
      Action = TabNextTab1
      Align = alRight
      TabOrder = 1
      ExplicitLeft = 1092
      ExplicitHeight = 44
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
    Left = 480
    Top = 176
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
    Left = 96
    Top = 240
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 504
    Top = 496
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 424
    Top = 104
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
    Left = 616
    Top = 408
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 616
    Top = 496
  end
  object FDPhysMySQLDriverLink2: TFDPhysMySQLDriverLink
    Left = 504
    Top = 568
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
    Top = 488
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
    Left = 192
    Top = 488
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
    Top = 40
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
  end
  object ActionList1: TActionList
    Images = ImageListMainMenu
    Left = 824
    Top = 160
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
      Caption = 'Save Generated Code'
      ImageIndex = 4
      OnExecute = actSaveGeneratedCodeExecute
    end
    object actGenerateCode: TAction
      Caption = 'Generate Code'
      ImageIndex = 5
      OnExecute = actGenerateCodeExecute
      OnUpdate = actGenerateCodeUpdate
    end
    object actRefreshCatalog: TAction
      Caption = 'Refresh Catalog'
      ImageIndex = 6
      OnExecute = actRefreshCatalogExecute
    end
    object actRefreshTableList: TAction
      Caption = 'Refresh Table List'
      ImageIndex = 6
      OnExecute = actRefreshTableListExecute
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
    Left = 688
    Top = 144
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
    Left = 840
    Top = 72
    Bitmap = {
      494C010101000800040010001000FFFFFFFF0510FFFFFFFFFFFFFFFF424D7600
      0000000000007600000028000000400000001000000001000400000000000002
      0000000000000000000000000000000000000000000000008000008000000080
      800080000000800080008080000080808000C0C0C0000000FF0000FF000000FF
      FF00FF000000FF00FF00FFFF0000FFFFFF000000830000680000000000000000
      00000000000000000000000000000000000000F307F00F706F00000000000000
      0000000000000000000000000000000000000F16F000000F34F0000000000000
      0000000000000000000000000000000000000360000000000360000000000000
      00000000000000000000000000000000000084F0000000000F18000000000000
      0000000000000000000000000000000000003600000000000036000000000000
      0000000000000000000000000000000000001800000000000084000000000000
      0000000000000000000000000000000000005F000000000000F5000000000000
      0000000000000000000000000000000000005F000000000000F5000000000000
      0000000000000000000000000000000000001800000000000084000000000000
      0000000000000000000000000000000000003600000000000036000000000000
      00000000000000000000000000000000000084F0000000000F18000000000000
      0000000000000000000000000000000000000360F5F000000360000000000000
      0000000000000000000000000000000000000F1685F0000F34F0000000000000
      00000000000000000000000000000000000000F304F00F706F00000000000000
      0000000000000000000000000000000000000F1004F00F780000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F00F000000000000C183000000000000
      87E10000000000009FF90000000000001FF80000000000003FFC000000000000
      3FFC0000000000003FFC0000000000003FFC0000000000003FFC000000000000
      3FFC0000000000001FF800000000000091F900000000000081E1000000000000
      C183000000000000818F00000000000000000000000000000000000000000000
      000000000000}
  end
  object qryMeta: TFDMetaInfoQuery
    Connection = FDConnection
    MetaInfoKind = mkTableFields
    Left = 96
    Top = 176
  end
end
