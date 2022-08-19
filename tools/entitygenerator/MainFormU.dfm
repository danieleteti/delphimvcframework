object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = '[DMVCFramework] MVCActiveRecord Entity Generator'
  ClientHeight = 688
  ClientWidth = 1199
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
    Width = 1199
    Height = 638
    ActivePage = tsGeneratedCode
    Align = alClient
    TabOrder = 0
    object tsConnectionDefinition: TTabSheet
      Caption = 'Connection Definition'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 89
        Width = 1191
        Height = 513
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
          Width = 1183
          Height = 13
          Align = alTop
          Caption = 'FireDAC connection parameters'
          ExplicitWidth = 152
        end
        object mmConnectionParams: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 23
          Width = 785
          Height = 445
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
          Left = 792
          Top = 20
          Width = 398
          Height = 451
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
            Height = 445
            Align = alClient
            Caption = 'Select a catalog and then the schema where your tables are'
            Padding.Left = 5
            Padding.Top = 5
            Padding.Right = 5
            Padding.Bottom = 5
            TabOrder = 0
            object lstSchema: TListBox
              AlignWithMargins = True
              Left = 191
              Top = 50
              Width = 191
              Height = 385
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
            object lstCatalog: TListBox
              AlignWithMargins = True
              Left = 10
              Top = 50
              Width = 175
              Height = 385
              Align = alLeft
              ItemHeight = 13
              TabOrder = 1
              OnClick = lstCatalogClick
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
          Top = 471
          Width = 1189
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'Panel9'
          ShowCaption = False
          TabOrder = 2
          object btnRefreshCatalog: TButton
            AlignWithMargins = True
            Left = 1068
            Top = 3
            Width = 118
            Height = 35
            Action = actRefreshCatalog
            Align = alRight
            TabOrder = 0
          end
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1191
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
          Left = 599
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
          ExplicitLeft = 593
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
        Width = 1191
        Height = 602
        Align = alClient
        Caption = 'Panel3'
        TabOrder = 0
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 1189
          Height = 208
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Panel4'
          ShowCaption = False
          TabOrder = 0
          DesignSize = (
            1189
            208)
          object btnGenEntities: TButton
            AlignWithMargins = True
            Left = 1242
            Top = 57
            Width = 161
            Height = 35
            Anchors = [akRight, akBottom]
            Caption = 'Generate Entities'
            TabOrder = 0
          end
          object chkGenerateMapping: TCheckBox
            AlignWithMargins = True
            Left = 10
            Top = 173
            Width = 1176
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
          Top = 260
          Width = 1183
          Height = 338
          ActivePage = TabSheet1
          Align = alClient
          TabOrder = 1
          object TabSheet1: TTabSheet
            Caption = 'Tables'
            object DBGrid1: TDBGrid
              Left = 0
              Top = 41
              Width = 1175
              Height = 261
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
              Width = 1175
              Height = 41
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
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
          Width = 1189
          Height = 48
          Align = alTop
          Caption = 'Panel10'
          ShowCaption = False
          TabOrder = 2
          object SpeedButton1: TSpeedButton
            AlignWithMargins = True
            Left = 764
            Top = 6
            Width = 133
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alRight
            Caption = 'Select All'
            OnClick = SpeedButton1Click
            ExplicitLeft = 129
            ExplicitTop = 3
            ExplicitHeight = 29
          end
          object SpeedButton2: TSpeedButton
            AlignWithMargins = True
            Left = 907
            Top = 6
            Width = 133
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alRight
            Caption = 'Select None'
            OnClick = SpeedButton2Click
            ExplicitLeft = 267
            ExplicitTop = 3
            ExplicitHeight = 29
          end
          object SpeedButton3: TSpeedButton
            AlignWithMargins = True
            Left = 1050
            Top = 6
            Width = 133
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alRight
            Caption = 'Invert Selection'
            OnClick = SpeedButton3Click
            ExplicitLeft = 405
            ExplicitTop = 3
            ExplicitHeight = 29
          end
          object btnGetTables: TButton
            AlignWithMargins = True
            Left = 6
            Top = 6
            Width = 163
            Height = 36
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alLeft
            Caption = 'Refresh Table List'
            TabOrder = 0
          end
        end
      end
    end
    object tsGeneratedCode: TTabSheet
      Caption = 'Generated Code'
      ImageIndex = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 1191
        Height = 41
        Align = alTop
        Caption = 'Panel5'
        ShowCaption = False
        TabOrder = 0
        object btnSaveCode: TButton
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 189
          Height = 33
          Action = actSaveGeneratedCode
          Align = alLeft
          TabOrder = 0
        end
      end
      object mmOutput: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 1185
        Height = 555
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 638
    Width = 1199
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnPrev: TButton
      AlignWithMargins = True
      Left = 982
      Top = 3
      Width = 104
      Height = 44
      Action = TabPreviousTab1
      Align = alRight
      TabOrder = 0
    end
    object btnNext: TButton
      AlignWithMargins = True
      Left = 1092
      Top = 3
      Width = 104
      Height = 44
      Action = TabNextTab1
      Align = alRight
      TabOrder = 1
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
    Left = 304
    Top = 152
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
    Left = 688
    Top = 232
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
    Left = 808
    Top = 240
    object actLoadProject: TAction
      Caption = 'Load Project'
      OnExecute = actLoadProjectExecute
    end
    object actSaveProject: TAction
      Caption = 'Save Project'
      OnExecute = actSaveProjectExecute
    end
    object actSaveProjectAs: TAction
      Caption = 'Save project as...'
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
      OnExecute = actSaveGeneratedCodeExecute
      OnUpdate = actSaveGeneratedCodeUpdate
    end
    object actGenerateCode: TAction
      Caption = 'Generate Code'
      OnExecute = actGenerateCodeExecute
    end
    object actRefreshCatalog: TAction
      Caption = 'Refresh Catalog'
      OnExecute = actRefreshCatalogExecute
    end
    object actRefreshTableList: TAction
      Caption = 'Refresh Table List'
      OnExecute = actRefreshTableListExecute
    end
    object actNewProject: TAction
      Caption = 'New Project'
      OnExecute = actNewProjectExecute
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
end
