object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 482
  ClientWidth = 535
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
    Left = 16
    Top = 24
    Width = 89
    Height = 21
    TabOrder = 0
    Text = '42'
  end
  object edtValue2: TEdit
    Left = 111
    Top = 24
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '10'
  end
  object btnSubstract: TButton
    Left = 206
    Top = 22
    Width = 91
    Height = 25
    Caption = 'Subtract'
    TabOrder = 2
    OnClick = btnSubstractClick
  end
  object edtResult: TEdit
    Left = 303
    Top = 24
    Width = 74
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object edtReverseString: TEdit
    Left = 16
    Top = 72
    Width = 184
    Height = 21
    TabOrder = 4
    Text = 'Daniele Teti'
  end
  object btnReverseString: TButton
    Left = 206
    Top = 70
    Width = 91
    Height = 25
    Caption = 'Reverse String'
    TabOrder = 5
    OnClick = btnReverseStringClick
  end
  object edtReversedString: TEdit
    Left = 303
    Top = 72
    Width = 210
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object edtFilter: TEdit
    Left = 16
    Top = 128
    Width = 184
    Height = 21
    TabOrder = 7
  end
  object edtGetCustomers: TButton
    Left = 206
    Top = 126
    Width = 91
    Height = 25
    Caption = 'Get Customers'
    TabOrder = 8
    OnClick = edtGetCustomersClick
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 157
    Width = 497
    Height = 317
    DataSource = DataSource1
    TabOrder = 9
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 264
    Top = 248
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 264
    Top = 312
    object FDMemTable1Code: TIntegerField
      FieldName = 'Code'
    end
    object FDMemTable1Name: TStringField
      FieldName = 'Name'
    end
  end
end
