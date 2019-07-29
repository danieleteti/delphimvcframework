object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 448
  ClientWidth = 831
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 16
    Width = 815
    Height = 124
    Caption = 'Simple Types'
    TabOrder = 0
    object edtValue1: TEdit
      Left = 17
      Top = 32
      Width = 70
      Height = 21
      TabOrder = 0
      Text = '42'
    end
    object edtValue2: TEdit
      Left = 93
      Top = 32
      Width = 70
      Height = 21
      TabOrder = 1
      Text = '10'
    end
    object btnSubstract: TButton
      Left = 169
      Top = 30
      Width = 100
      Height = 25
      Caption = 'Subtract'
      TabOrder = 2
      OnClick = btnSubstractClick
    end
    object edtResult: TEdit
      Left = 273
      Top = 32
      Width = 70
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object edtReverseString: TEdit
      Left = 17
      Top = 80
      Width = 144
      Height = 21
      TabOrder = 4
      Text = 'Daniele Teti'
    end
    object btnReverseString: TButton
      Left = 167
      Top = 78
      Width = 100
      Height = 25
      Caption = 'Reverse String'
      TabOrder = 5
      OnClick = btnReverseStringClick
    end
    object edtReversedString: TEdit
      Left = 273
      Top = 80
      Width = 178
      Height = 21
      ReadOnly = True
      TabOrder = 6
    end
    object dtNextMonday: TDateTimePicker
      Left = 564
      Top = 16
      Width = 102
      Height = 21
      Date = 43018.000000000000000000
      Time = 0.469176562502980200
      TabOrder = 7
    end
    object btnAddDay: TButton
      Left = 672
      Top = 14
      Width = 128
      Height = 25
      Caption = 'Get Next Monday'
      TabOrder = 8
      OnClick = btnAddDayClick
    end
    object btnInvalid1: TButton
      Left = 638
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
      Left = 728
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
      Left = 476
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
      Left = 557
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
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 146
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
    Left = 509
    Top = 146
    Width = 314
    Height = 294
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
      Height = 204
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
    Left = 8
    Top = 311
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
