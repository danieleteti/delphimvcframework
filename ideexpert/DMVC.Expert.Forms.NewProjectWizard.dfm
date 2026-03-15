object frmDMVCNewProject: TfrmDMVCNewProject
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DMVCFramework :: New Project Wizard'
  ClientHeight = 672
  ClientWidth = 764
  Color = clBtnFace
  Constraints.MinHeight = 145
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 764
    Height = 121
    Align = alTop
    Pen.Color = clWhite
  end
  object Image1: TImage
    Left = 24
    Top = 8
    Width = 352
    Height = 101
    Cursor = crHandPoint
    Hint = 'Go to the DelphiMVCFramework project'
    AutoSize = True
    Center = True
    OnClick = Image1Click
  end
  object lblFrameworkVersion: TLabel
    Left = 461
    Top = 100
    Width = 292
    Height = 16
    Cursor = crHandPoint
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'X.X.X carbonara'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = lblFrameworkVersionClick
    OnMouseEnter = lblFrameworkVersionMouseEnter
    OnMouseLeave = lblFrameworkVersionMouseLeave
  end
  object lblBook: TLabel
    AlignWithMargins = True
    Left = 494
    Top = 7
    Width = 259
    Height = 16
    Cursor = crHandPoint
    Hint = 'Go to DMVCFramework - the official guide'
    Margins.Right = 10
    Alignment = taRightJustify
    Anchors = []
    Caption = 'DelphiMVCFramework - the official guide'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    OnClick = lblBookClick
    OnMouseEnter = lblBookMouseEnter
    OnMouseLeave = lblBookMouseLeave
  end
  object lblCopyRight: TLabel
    Left = 24
    Top = 102
    Width = 330
    Height = 13
    Caption = 'Copyright (c) 2010-0000 Daniele Teti and the DMVCFramework Team'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Shape2: TShape
    Left = 497
    Top = 34
    Width = 256
    Height = 41
    Pen.Color = clGray
  end
  object Image2: TImage
    Left = 498
    Top = 34
    Width = 36
    Height = 41
    Center = True
    OnClick = lblPATREONClick
    OnMouseEnter = lblPATREONMouseEnter
    OnMouseLeave = lblPATREONMouseLeave
  end
  object lblPATREON: TLabel
    Left = 541
    Top = 37
    Width = 205
    Height = 41
    Cursor = crHandPoint
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = 'Premium videos, tutorials and articles on PATREON'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
    OnClick = lblPATREONClick
    OnMouseEnter = lblPATREONMouseEnter
    OnMouseLeave = lblPATREONMouseLeave
  end
  object lblPageTitle: TLabel
    Left = 24
    Top = 126
    Width = 716
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Application Type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPageHint: TLabel
    Left = 24
    Top = 152
    Width = 716
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Select the type of application to be created'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pcWizard: TPageControl
    Left = 0
    Top = 175
    Width = 764
    Height = 455
    ActivePage = tsAppType
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabHeight = 1
    TabOrder = 0
    TabWidth = 1
    object tsAppType: TTabSheet
      Caption = 'AppType'
      TabVisible = False
      object rgApplicationType: TRadioGroup
        Left = 24
        Top = 24
        Width = 400
        Height = 80
        Caption = 'Application Type'
        ItemIndex = 0
        Items.Strings = (
          'Console Application (Windows and Linux)'
          'Windows Service')
        TabOrder = 0
        OnClick = rgApplicationTypeClick
      end
      object lblAppTypeDescription: TLabel
        Left = 24
        Top = 120
        Width = 700
        Height = 80
        AutoSize = False
        Caption =
          'Runs in a terminal/console window. Ideal for development, debug' +
          'ging, and Docker/container deployments. Works on both Windows an' +
          'd Linux.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
    object tsServer: TTabSheet
      Caption = 'Server'
      TabVisible = False
      object lblServerPort: TLabel
        Left = 260
        Top = 24
        Width = 55
        Height = 13
        Caption = 'Server Port'
      end
      object lblProjectName: TLabel
        Left = 24
        Top = 150
        Width = 64
        Height = 13
        Caption = 'Project Name'
      end
      object lblProjectFolder: TLabel
        Left = 24
        Top = 200
        Width = 203
        Height = 13
        Caption = 'Base Folder (project created as subfolder)'
      end
      object rgServerProtocol: TRadioGroup
        Left = 24
        Top = 24
        Width = 220
        Height = 105
        Caption = 'Server Protocol'
        ItemIndex = 0
        Items.Strings = (
          'HTTP'
          'HTTPS (requires TaurusTLS)'
          'FastCGI')
        TabOrder = 0
      end
      object edtServerPort: TEdit
        Left = 260
        Top = 43
        Width = 80
        Height = 21
        TabOrder = 1
        Text = '8080'
        TextHint = '8080'
      end
      object edtProjectName: TEdit
        Left = 24
        Top = 169
        Width = 320
        Height = 21
        TabOrder = 2
        TextHint = 'DMVCFrameworkProject1'
      end
      object edtProjectFolder: TEdit
        Left = 24
        Top = 219
        Width = 290
        Height = 21
        TabOrder = 3
      end
      object btnBrowseFolder: TButton
        Left = 320
        Top = 217
        Width = 26
        Height = 25
        Caption = '...'
        TabOrder = 4
        OnClick = btnBrowseFolderClick
      end
    end
    object tsFeatures: TTabSheet
      Caption = 'Features'
      TabVisible = False
      object gbControllerUnitOptions: TGroupBox
        Left = 16
        Top = 8
        Width = 340
        Height = 130
        Caption = 'Controller Unit Options'
        TabOrder = 0
        object chkCreateIndexMethod: TCheckBox
          Left = 16
          Top = 25
          Width = 300
          Height = 17
          Caption = 'Generate Index And Sample Actions'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkCreateActionFiltersMethods: TCheckBox
          Left = 16
          Top = 48
          Width = 300
          Height = 17
          Caption = 'Generate Action Filters Methods'
          TabOrder = 1
        end
        object chkCreateCRUDMethods: TCheckBox
          Left = 16
          Top = 71
          Width = 300
          Height = 17
          Caption = 'Generate Sample CRUD Actions'
          TabOrder = 2
        end
        object chkProfileActions: TCheckBox
          Left = 16
          Top = 94
          Width = 300
          Height = 17
          Caption = 'Include Actions Profiling Code'
          TabOrder = 3
        end
      end
      object gbAdditionalFeatures: TGroupBox
        Left = 370
        Top = 8
        Width = 370
        Height = 176
        Caption = 'Additional Features'
        TabOrder = 1
        object lblSSV: TLabel
          Left = 16
          Top = 22
          Width = 167
          Height = 13
          Caption = 'Server Side Views Template Engine'
        end
        object lblSessionType: TLabel
          Left = 200
          Top = 22
          Width = 63
          Height = 13
          Caption = 'Session Type'
        end
        object lblJSONRPCClassName: TLabel
          Left = 200
          Top = 72
          Width = 148
          Height = 13
          Caption = 'JSON-RPC Handler Class Name'
        end
        object cbSSV: TComboBox
          Left = 16
          Top = 40
          Width = 170
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'None'
          Items.Strings = (
            'None'
            'TemplatePro'
            'WebStencils'
            'Mustache')
        end
        object cbSessionType: TComboBox
          Left = 200
          Top = 40
          Width = 155
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'None'
          Items.Strings = (
            'None'
            'Memory'
            'File'
            'Database')
        end
        object chkJSONRPC: TCheckBox
          Left = 16
          Top = 72
          Width = 170
          Height = 17
          Caption = 'Create JSON-RPC 2.0 end-point'
          TabOrder = 2
        end
        object EdtJSONRPCClassName: TEdit
          Left = 200
          Top = 90
          Width = 155
          Height = 21
          TabOrder = 3
          TextHint = 'TMyRPC'
        end
        object chkWebSocketServer: TCheckBox
          Left = 16
          Top = 120
          Width = 200
          Height = 17
          Caption = 'Create Web Socket Server'
          TabOrder = 4
        end
        object chkServicesContainer: TCheckBox
          Left = 16
          Top = 145
          Width = 200
          Height = 17
          Caption = 'Use Services Container'
          TabOrder = 5
        end
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 195
        Width = 724
        Height = 200
        Caption = 'Middlewares'
        TabOrder = 2
        object Label4: TLabel
          Left = 31
          Top = 145
          Width = 144
          Height = 13
          Caption = 'FireDAC Connections filename'
        end
        object Bevel1: TBevel
          Left = 11
          Top = 108
          Width = 700
          Height = 3
          Shape = bsTopLine
        end
        object Label5: TLabel
          Left = 228
          Top = 145
          Width = 101
          Height = 13
          Caption = 'ConnectionDef Name'
        end
        object chkCompression: TCheckBox
          Left = 28
          Top = 27
          Width = 130
          Height = 17
          Caption = 'Compression'
          TabOrder = 0
        end
        object chkAnalyticsMiddleware: TCheckBox
          Left = 28
          Top = 50
          Width = 130
          Height = 17
          Caption = 'Analytics'
          TabOrder = 1
        end
        object chkStaticFiles: TCheckBox
          Left = 28
          Top = 74
          Width = 130
          Height = 17
          Caption = 'Static Files'
          TabOrder = 2
        end
        object chkCORS: TCheckBox
          Left = 170
          Top = 27
          Width = 130
          Height = 17
          Caption = 'CORS'
          TabOrder = 3
        end
        object chkTrace: TCheckBox
          Left = 170
          Top = 50
          Width = 130
          Height = 17
          Hint = 'Debug purposes'
          Caption = 'Tracing (debug)'
          TabOrder = 4
        end
        object chkETAG: TCheckBox
          Left = 170
          Top = 74
          Width = 130
          Height = 17
          Caption = 'ETag'
          TabOrder = 5
        end
        object chkRateLimit: TCheckBox
          Left = 310
          Top = 27
          Width = 130
          Height = 17
          Caption = 'Rate Limit'
          TabOrder = 6
        end
        object chkJWT: TCheckBox
          Left = 310
          Top = 50
          Width = 130
          Height = 17
          Caption = 'JWT (Cookie)'
          TabOrder = 7
        end
        object chkActiveRecord: TCheckBox
          Left = 28
          Top = 121
          Width = 135
          Height = 17
          Caption = 'ActiveRecord'
          TabOrder = 8
        end
        object EdtFDConnDefFileName: TEdit
          Left = 31
          Top = 164
          Width = 180
          Height = 21
          TabOrder = 9
          Text = 'FDConnectionDefs.ini'
          TextHint = 'FDConnectionDefs.ini'
        end
        object EdtConnDefName: TEdit
          Left = 228
          Top = 164
          Width = 180
          Height = 21
          TabOrder = 10
          Text = 'MyConnDef'
          TextHint = 'MyConnDef'
        end
      end
    end
    object tsOptions: TTabSheet
      Caption = 'Options'
      TabVisible = False
      object lblNameCase: TLabel
        Left = 24
        Top = 16
        Width = 400
        Height = 13
        Caption = 'Default name case for properties serialization (MVCNameCaseDefault)'
      end
      object lblClassName: TLabel
        Left = 24
        Top = 68
        Width = 105
        Height = 13
        Caption = 'Controller Class Name'
      end
      object lblWbModule: TLabel
        Left = 24
        Top = 118
        Width = 114
        Height = 13
        Caption = 'WebModule Class Name'
      end
      object cbNameCase: TComboBox
        Left = 24
        Top = 35
        Width = 393
        Height = 21
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 0
        Text = 'LowerCase (foobar)'
        Items.Strings = (
          'AsIs (as declared)'
          'UpperCase (FOOBAR)'
          'LowerCase (foobar)'
          'CamelCase (fooBar)'
          'PascalCase (FooBar)'
          'SnakeCase (foo_bar)')
      end
      object edtControllerClassName: TEdit
        Left = 24
        Top = 87
        Width = 265
        Height = 21
        TabOrder = 1
      end
      object edtWebModuleName: TEdit
        Left = 24
        Top = 137
        Width = 228
        Height = 21
        TabOrder = 2
      end
      object chkMSHeap: TCheckBox
        Left = 24
        Top = 180
        Width = 200
        Height = 17
        Caption = 'Use MSHeap on MS Windows'
        TabOrder = 3
      end
      object chkSqids: TCheckBox
        Left = 250
        Top = 180
        Width = 200
        Height = 17
        Caption = 'Use Sqids'
        TabOrder = 4
      end
      object chkCustomConfigDotEnv: TCheckBox
        Left = 24
        Top = 206
        Width = 250
        Height = 17
        Caption = 'Generate custom .env configuration'
        TabOrder = 5
      end
    end
  end
  object btnBack: TButton
    Left = 430
    Top = 637
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = '<< Back'
    TabOrder = 1
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 513
    Top = 637
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = 'Next >>'
    Default = True
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnFinish: TButton
    Left = 596
    Top = 637
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = 'Finish'
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = btnFinishClick
  end
  object btnCancel: TButton
    Left = 679
    Top = 637
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 408
    Top = 64
  end
end
