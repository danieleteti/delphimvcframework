object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TDataSet Helpers'
  ClientHeight = 490
  ClientWidth = 1230
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 25
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1230
    Height = 73
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object btnSaveDataSet: TButton
      AlignWithMargins = True
      Left = 536
      Top = 4
      Width = 267
      Height = 65
      Align = alLeft
      Caption = 'Save Dataset as JSON Array'
      TabOrder = 0
      OnClick = btnSaveDataSetClick
      ExplicitLeft = 844
      ExplicitTop = 8
    end
    object btnToList: TButton
      AlignWithMargins = True
      Left = 266
      Top = 4
      Width = 264
      Height = 65
      Align = alLeft
      Caption = 'Dataset To ObjectList<T>'
      TabOrder = 1
      OnClick = btnToListClick
      ExplicitLeft = 789
      ExplicitTop = 20
    end
    object btnLoadFromAPI: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 256
      Height = 65
      Align = alLeft
      Caption = 'Load From API'
      TabOrder = 2
      OnClick = btnLoadFromAPIClick
      ExplicitLeft = 547
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 76
    Width = 1224
    Height = 411
    Align = alClient
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -19
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'id'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'userId'
        Title.Caption = 'User ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'title'
        Width = 522
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'completed'
        Width = 113
        Visible = True
      end>
  end
  object Connection: TFDConnection
    Params.Strings = (
      'ConnectionDef=employee_fb')
    LoginPrompt = False
    Left = 31
    Top = 66
  end
  object CustomerTable: TFDQuery
    Connection = Connection
    SQL.Strings = (
      'SELECT * FROM CUSTOMER')
    Left = 31
    Top = 130
  end
  object FDMemTable1: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'userId'
        DataType = ftInteger
      end
      item
        Name = 'title'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'completed'
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
    Left = 448
    Top = 176
    object FDMemTable1id: TIntegerField
      DisplayLabel = '#ID'
      DisplayWidth = 10
      FieldName = 'id'
    end
    object FDMemTable1userId: TIntegerField
      FieldName = 'userId'
    end
    object FDMemTable1title: TStringField
      DisplayLabel = 'Title'
      DisplayWidth = 39
      FieldName = 'title'
      Size = 50
    end
    object FDMemTable1completed: TBooleanField
      DisplayLabel = 'Completed'
      DisplayWidth = 12
      FieldName = 'completed'
      DisplayValues = 'Yes;No'
    end
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = FDMemTable1
    Left = 448
    Top = 240
  end
end
