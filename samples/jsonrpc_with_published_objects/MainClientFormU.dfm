object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JSON-RPC 2.0 Client'
  ClientHeight = 527
  ClientWidth = 842
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 842
    Height = 527
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Invoking Plain PODO'
      object GroupBox1: TGroupBox
        Left = 3
        Top = 22
        Width = 815
        Height = 124
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
        object btnSubstract: TButton
          Left = 87
          Top = 30
          Width = 100
          Height = 25
          Caption = 'Subtract'
          TabOrder = 2
          OnClick = btnSubstractClick
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
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 162
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
      object GroupBox3: TGroupBox
        Left = 504
        Top = 162
        Width = 314
        Height = 310
        Caption = 'Returning Datasets'
        TabOrder = 2
        object edtFilter: TEdit
          Left = 18
          Top = 32
          Width = 184
          Height = 21
          TabOrder = 0
        end
        object edtGetCustomers: TButton
          Left = 208
          Top = 30
          Width = 91
          Height = 25
          Caption = 'Get Customers'
          TabOrder = 1
          OnClick = edtGetCustomersClick
        end
        object DBGrid1: TDBGrid
          Left = 18
          Top = 61
          Width = 279
          Height = 236
          DataSource = DataSource1
          TabOrder = 2
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
      end
      object GroupBox4: TGroupBox
        Left = 3
        Top = 343
        Width = 489
        Height = 129
        Caption = 'Passing Objects as parameters'
        TabOrder = 3
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
    end
    object TabSheet2: TTabSheet
      Caption = 'Invoking DataModule Methods'
      ImageIndex = 1
      object GroupBox5: TGroupBox
        Left = 11
        Top = 18
        Width = 489
        Height = 159
        Caption = 'Returning Objects'
        TabOrder = 0
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
    end
  end
  object btnWithJSON: TButton
    Left = 552
    Top = 76
    Width = 75
    Height = 25
    Caption = 'JSON Prop'
    TabOrder = 1
    OnClick = btnWithJSONClick
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 767
    Top = 184
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
    Top = 248
    object FDMemTable1Code: TIntegerField
      FieldName = 'Code'
    end
    object FDMemTable1Name: TStringField
      FieldName = 'Name'
    end
  end
end
