object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 544
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtValue1: TEdit
    Left = 8
    Top = 8
    Width = 89
    Height = 21
    TabOrder = 0
    Text = '42'
  end
  object edtValue2: TEdit
    Left = 103
    Top = 8
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '10'
  end
  object btnSubstract: TButton
    Left = 198
    Top = 6
    Width = 91
    Height = 25
    Caption = 'Subtract'
    TabOrder = 2
    OnClick = btnSubstractClick
  end
  object edtResult: TEdit
    Left = 295
    Top = 8
    Width = 74
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object edtReverseString: TEdit
    Left = 8
    Top = 56
    Width = 184
    Height = 21
    TabOrder = 4
    Text = 'Daniele Teti'
  end
  object btnReverseString: TButton
    Left = 198
    Top = 54
    Width = 91
    Height = 25
    Caption = 'Reverse String'
    TabOrder = 5
    OnClick = btnReverseStringClick
  end
  object edtReversedString: TEdit
    Left = 295
    Top = 56
    Width = 202
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object edtFilter: TEdit
    Left = 8
    Top = 245
    Width = 184
    Height = 21
    TabOrder = 7
  end
  object edtGetCustomers: TButton
    Left = 198
    Top = 243
    Width = 91
    Height = 25
    Caption = 'Get Customers'
    TabOrder = 8
    OnClick = edtGetCustomersClick
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 272
    Width = 489
    Height = 264
    DataSource = DataSource1
    TabOrder = 9
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object edtUserName: TEdit
    Left = 8
    Top = 104
    Width = 184
    Height = 21
    TabOrder = 10
    Text = 'dteti'
  end
  object btnGetUser: TButton
    Left = 198
    Top = 102
    Width = 91
    Height = 25
    Caption = 'Get User'
    TabOrder = 11
    OnClick = btnGetUserClick
  end
  object lbPerson: TListBox
    Left = 8
    Top = 133
    Width = 489
    Height = 82
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 256
    Top = 280
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 256
    Top = 344
    object FDMemTable1Code: TIntegerField
      FieldName = 'Code'
    end
    object FDMemTable1Name: TStringField
      FieldName = 'Name'
    end
  end
end
