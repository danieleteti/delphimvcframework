object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JSON-RPC 2.0 Client'
  ClientHeight = 604
  ClientWidth = 842
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 842
    Height = 604
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 838
    ExplicitHeight = 603
    object TabSheet1: TTabSheet
      Caption = 'Invoking Plain PODO'
      object GroupBox1: TGroupBox
        Left = 3
        Top = 22
        Width = 815
        Height = 174
        Caption = 'Simple Types'
        TabOrder = 0
        object edtValue1: TEdit
          Left = 17
          Top = 32
          Width = 32
          Height = 21
          TabOrder = 0
          Text = '42'
        end
        object edtValue2: TEdit
          Left = 55
          Top = 32
          Width = 26
          Height = 21
          TabOrder = 1
          Text = '10'
        end
        object btnSubtract: TButton
          Left = 87
          Top = 30
          Width = 100
          Height = 25
          Caption = 'Subtract'
          TabOrder = 2
          OnClick = btnSubtractClick
        end
        object edtResult: TEdit
          Left = 193
          Top = 32
          Width = 27
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtReverseString: TEdit
          Left = 17
          Top = 80
          Width = 88
          Height = 21
          TabOrder = 4
          Text = 'Daniele Teti'
        end
        object btnReverseString: TButton
          Left = 111
          Top = 78
          Width = 109
          Height = 25
          Caption = 'Reverse String'
          TabOrder = 5
          OnClick = btnReverseStringClick
        end
        object edtReversedString: TEdit
          Left = 320
          Top = 80
          Width = 131
          Height = 21
          ReadOnly = True
          TabOrder = 6
        end
        object dtNextMonday: TDateTimePicker
          Left = 253
          Top = 32
          Width = 102
          Height = 21
          Date = 43018.000000000000000000
          Time = 0.469176562502980200
          TabOrder = 7
        end
        object btnAddDay: TButton
          Left = 361
          Top = 30
          Width = 104
          Height = 25
          Caption = 'Get Next Monday'
          TabOrder = 8
          OnClick = btnAddDayClick
        end
        object btnInvalid1: TButton
          Left = 626
          Top = 78
          Width = 84
          Height = 43
          Caption = 'Passing VAR parameters'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clScrollBar
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          WordWrap = True
          OnClick = btnInvalid1Click
        end
        object btnInvalid2: TButton
          Left = 716
          Top = 78
          Width = 84
          Height = 43
          Caption = 'Passing OUT parameters'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clScrollBar
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
          WordWrap = True
          OnClick = btnInvalid2Click
        end
        object btnNotification: TButton
          Left = 464
          Top = 78
          Width = 75
          Height = 43
          Caption = 'Send Notification'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clScrollBar
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          WordWrap = True
          OnClick = btnNotificationClick
        end
        object btnInvalidMethod: TButton
          Left = 545
          Top = 78
          Width = 75
          Height = 43
          Caption = 'Invalid Method'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clScrollBar
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
          WordWrap = True
          OnClick = btnInvalidMethodClick
        end
        object CheckBox1: TCheckBox
          Left = 226
          Top = 82
          Width = 88
          Height = 17
          Caption = 'As Uppercase'
          TabOrder = 13
        end
        object btnDates: TButton
          Left = 716
          Top = 30
          Width = 84
          Height = 25
          Caption = 'PlayWithDates'
          TabOrder = 14
          OnClick = btnDatesClick
        end
        object btnFloatsTests: TButton
          Left = 626
          Top = 30
          Width = 84
          Height = 25
          Caption = 'Floats'
          TabOrder = 15
          OnClick = btnFloatsTestsClick
        end
        object btnWithJSON: TButton
          Left = 545
          Top = 30
          Width = 75
          Height = 25
          Caption = 'JSON Prop'
          TabOrder = 16
          OnClick = btnWithJSONClick
        end
        object Edit1: TEdit
          Left = 17
          Top = 136
          Width = 32
          Height = 21
          TabOrder = 17
          Text = '42'
        end
        object Edit2: TEdit
          Left = 55
          Top = 136
          Width = 26
          Height = 21
          TabOrder = 18
          Text = '10'
        end
        object btnSubtractWithNamedParams: TButton
          Left = 87
          Top = 134
          Width = 160
          Height = 25
          Caption = 'Subtract (named params)'
          TabOrder = 19
          OnClick = btnSubtractWithNamedParamsClick
        end
        object Edit3: TEdit
          Left = 253
          Top = 136
          Width = 27
          Height = 21
          ReadOnly = True
          TabOrder = 20
        end
        object btnGenericException: TButton
          Left = 464
          Top = 127
          Width = 156
          Height = 32
          Caption = 'Raise Generic Exception'
          TabOrder = 21
          OnClick = btnGenericExceptionClick
        end
        object btnException: TButton
          Left = 626
          Top = 127
          Width = 170
          Height = 32
          Caption = 'Raise Custom Exception'
          TabOrder = 22
          OnClick = btnExceptionClick
        end
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 202
        Width = 489
        Height = 159
        Caption = 'Returning Objects'
        TabOrder = 1
        object edtUserName: TEdit
          Left = 16
          Top = 24
          Width = 184
          Height = 21
          TabOrder = 0
          Text = 'dteti'
        end
        object btnGetUser: TButton
          Left = 206
          Top = 22
          Width = 91
          Height = 25
          Caption = 'Get User'
          TabOrder = 1
          OnClick = btnGetUserClick
        end
        object lbPerson: TListBox
          Left = 16
          Top = 53
          Width = 435
          Height = 82
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
      object GroupBox4: TGroupBox
        Left = 3
        Top = 383
        Width = 489
        Height = 129
        Caption = 'Passing Objects as parameters'
        TabOrder = 2
        object edtFirstName: TLabeledEdit
          Left = 16
          Top = 40
          Width = 121
          Height = 21
          EditLabel.Width = 51
          EditLabel.Height = 13
          EditLabel.Caption = 'First Name'
          TabOrder = 0
          Text = 'Daniele'
        end
        object edtLastName: TLabeledEdit
          Left = 16
          Top = 88
          Width = 121
          Height = 21
          EditLabel.Width = 50
          EditLabel.Height = 13
          EditLabel.Caption = 'Last Name'
          TabOrder = 1
          Text = 'Teti'
        end
        object chkMarried: TCheckBox
          Left = 172
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Married'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object dtDOB: TDateTimePicker
          Left = 169
          Top = 88
          Width = 102
          Height = 21
          Date = 29163.000000000000000000
          Time = 0.469176562499342300
          TabOrder = 3
        end
        object btnSave: TButton
          Left = 376
          Top = 88
          Width = 75
          Height = 25
          Caption = 'Save'
          TabOrder = 4
          OnClick = btnSaveClick
        end
      end
      object PageControl2: TPageControl
        Left = 514
        Top = 202
        Width = 304
        Height = 367
        ActivePage = TabSheet4
        TabOrder = 3
        object TabSheet3: TTabSheet
          Caption = 'Get DataSet'
          object edtFilter: TEdit
            Left = 3
            Top = 5
            Width = 184
            Height = 21
            TabOrder = 0
          end
          object edtGetCustomers: TButton
            Left = 193
            Top = 3
            Width = 91
            Height = 25
            Caption = 'Get Customers'
            TabOrder = 1
            OnClick = edtGetCustomersClick
          end
          object DBGrid1: TDBGrid
            Left = 3
            Top = 34
            Width = 279
            Height = 302
            DataSource = DataSource1
            TabOrder = 2
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Get Multi Dataset'
          ImageIndex = 1
          object btnGetMulti: TButton
            Left = 13
            Top = 16
            Width = 268
            Height = 41
            Caption = 'Get Multiple Datasets'
            TabOrder = 0
            OnClick = btnGetMultiClick
          end
          object lbMulti: TListBox
            Left = 16
            Top = 63
            Width = 265
            Height = 266
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ItemHeight = 14
            ParentFont = False
            TabOrder = 1
          end
        end
      end
      object btnSet: TButton
        Left = 379
        Top = 536
        Width = 75
        Height = 25
        Caption = 'Using Sets'
        TabOrder = 4
        OnClick = btnSetClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Invoking DataModule Methods'
      ImageIndex = 1
      object GroupBox5: TGroupBox
        Left = 11
        Top = 18
        Width = 489
        Height = 391
        Caption = 'Returning Objects'
        TabOrder = 0
        DesignSize = (
          489
          391)
        object edtSearchText: TEdit
          Left = 16
          Top = 24
          Width = 184
          Height = 21
          TabOrder = 0
          Text = 'pizz'
        end
        object btnSearch: TButton
          Left = 206
          Top = 22
          Width = 91
          Height = 25
          Caption = 'Search Article'
          TabOrder = 1
          OnClick = btnSearchClick
        end
        object ListBox1: TListBox
          Left = 16
          Top = 53
          Width = 435
          Height = 316
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Custom Exceptions Handling'
      ImageIndex = 2
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 808
        Height = 69
        Align = alTop
        Caption = 
          'If an exception raised by the serve doesn'#39't inherith from EMVCJS' +
          'ONRPCErrorResponse can be handled by a custom global exception b' +
          'lock. This custom handling can modify error code, error message ' +
          'and can add a custom data property to the exception.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object btnGenericExcWithCustomHandling: TButton
        Left = 0
        Top = 103
        Width = 217
        Height = 82
        Caption = 'Raise Generic Exception with custom handling (DATA is a String)'
        TabOrder = 0
        WordWrap = True
        OnClick = btnGenericExcWithCustomHandlingClick
      end
      object btnGenericExcWithCustomHAndling2: TButton
        Left = 223
        Top = 103
        Width = 217
        Height = 82
        Caption = 
          'Raise Generic Exception with custom handling (DATA is a JSONObje' +
          'ct)'
        TabOrder = 1
        WordWrap = True
        OnClick = btnGenericExcWithCustomHAndling2Click
      end
      object btnGenericExcWithoutCustomHandling: TButton
        Left = 446
        Top = 103
        Width = 217
        Height = 82
        Caption = 'Raise Generic Exception without custom handling'
        TabOrder = 2
        WordWrap = True
        OnClick = btnGenericExcWithoutCustomHandlingClick
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Using record as parameters'
      ImageIndex = 3
      DesignSize = (
        834
        576)
      object btnSingleRec: TButton
        Left = 16
        Top = 16
        Width = 185
        Height = 41
        Caption = 'Returning Single Record'
        TabOrder = 0
        OnClick = btnSingleRecClick
      end
      object lbLogRec: TMemo
        Left = 216
        Top = 16
        Width = 589
        Height = 545
        Anchors = [akLeft, akTop, akRight, akBottom]
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
      object btnGetArrayOfRecords: TButton
        Left = 16
        Top = 63
        Width = 185
        Height = 40
        Caption = 'Returning Array of Records'
        TabOrder = 2
        OnClick = btnGetArrayOfRecordsClick
      end
      object btnGetDynArray: TButton
        Left = 16
        Top = 109
        Width = 185
        Height = 40
        Caption = 'Returning DynArray of Records'
        TabOrder = 3
        OnClick = btnGetDynArrayClick
      end
      object btnPassAndGetRecord: TButton
        Left = 16
        Top = 155
        Width = 185
        Height = 40
        Caption = 'Using record parameters'
        TabOrder = 4
        OnClick = btnPassAndGetRecordClick
      end
      object btnEchoComplexArray: TButton
        Left = 16
        Top = 201
        Width = 185
        Height = 40
        Caption = 'Using Array as Parameter'
        TabOrder = 5
        OnClick = btnEchoComplexArrayClick
      end
      object btnComplex: TButton
        Left = 16
        Top = 247
        Width = 185
        Height = 40
        Caption = 'Using parameter with multiple arrays'
        TabOrder = 6
        OnClick = btnComplexClick
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 455
    Top = 216
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 767
    Top = 328
    object FDMemTable1Code: TIntegerField
      FieldName = 'Code'
    end
    object FDMemTable1Name: TStringField
      FieldName = 'Name'
    end
  end
end
