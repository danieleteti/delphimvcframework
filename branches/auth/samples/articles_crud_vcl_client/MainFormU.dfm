object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Articles CRUD SAMPLE'
  ClientHeight = 391
  ClientWidth = 747
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 747
    Height = 43
    Align = alTop
    TabOrder = 0
    object btnGetListAsynch: TButton
      AlignWithMargins = True
      Left = 120
      Top = 6
      Width = 89
      Height = 31
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get List Asynch'
      TabOrder = 0
      OnClick = btnGetListAsynchClick
    end
    object btnGetListSynch: TButton
      AlignWithMargins = True
      Left = 11
      Top = 6
      Width = 89
      Height = 31
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get List'
      TabOrder = 1
      OnClick = Button1Click
    end
    object DBNavigator1: TDBNavigator
      Left = 222
      Top = 8
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 2
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 43
    Width = 747
    Height = 348
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'id'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'code'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'description'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'price'
        Visible = True
      end>
  end
  object FDMemTable1: TFDMemTable
    BeforePost = FDMemTable1BeforePost
    BeforeDelete = FDMemTable1BeforeDelete
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    StoreDefs = True
    Left = 136
    Top = 120
    object FDMemTable1id: TIntegerField
      FieldName = 'id'
    end
    object FDMemTable1code: TStringField
      FieldName = 'code'
    end
    object FDMemTable1description: TStringField
      FieldName = 'description'
      Size = 50
    end
    object FDMemTable1price: TCurrencyField
      FieldName = 'price'
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 312
    Top = 192
  end
end
