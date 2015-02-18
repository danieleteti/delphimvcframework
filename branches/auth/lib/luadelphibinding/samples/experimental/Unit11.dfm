object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Form11'
  ClientHeight = 522
  ClientWidth = 885
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 49
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 159
    Top = 8
    Width = 280
    Height = 49
    Caption = 'Load File'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 72
    Width = 431
    Height = 41
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 119
    Width = 179
    Height = 58
    Caption = 'Button4'
    TabOrder = 3
    OnClick = Button4Click
  end
  object DBGrid1: TDBGrid
    Left = 193
    Top = 119
    Width = 640
    Height = 354
    DataSource = DataSource1
    TabOrder = 4
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
        FieldName = 'first_name'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'last_name'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'born_date'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'is_male'
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 193
    Top = 479
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 5
  end
  object Button5: TButton
    Left = 439
    Top = 479
    Width = 75
    Height = 25
    Caption = 'Save CDS'
    TabOrder = 6
    OnClick = Button5Click
  end
  object ClientDataSet1: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 88
    Top = 208
    Data = {
      840000009619E0BD010000001800000005000000000003000000840002696404
      000100000000000A66697273745F6E616D650100490000000100055749445448
      020002006400096C6173745F6E616D6501004900000001000557494454480200
      0200640009626F726E5F6461746504000600000000000769735F6D616C650200
      0300000000000000}
    object ClientDataSet1id: TIntegerField
      DisplayWidth = 12
      FieldName = 'id'
    end
    object ClientDataSet1first_name: TStringField
      DisplayWidth = 30
      FieldName = 'first_name'
      Size = 100
    end
    object ClientDataSet1last_name: TStringField
      DisplayWidth = 31
      FieldName = 'last_name'
      Size = 100
    end
    object ClientDataSet1born_date: TDateField
      DisplayWidth = 12
      FieldName = 'born_date'
    end
    object ClientDataSet1is_male: TBooleanField
      DisplayWidth = 14
      FieldName = 'is_male'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 88
    Top = 264
  end
end
