object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Articles CRUD SAMPLE'
  ClientHeight = 391
  ClientWidth = 669
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
    Width = 669
    Height = 39
    Align = alTop
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 378
      Top = 4
      Width = 287
      Height = 31
      DataSource = dsrcArticles
      Align = alRight
      TabOrder = 0
    end
    object btnOpen: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 75
      Height = 31
      Align = alLeft
      Caption = 'Open'
      TabOrder = 1
      OnClick = btnOpenClick
    end
    object btnRefreshRecord: TButton
      AlignWithMargins = True
      Left = 166
      Top = 4
      Width = 107
      Height = 31
      Align = alLeft
      Caption = 'Refresh Record'
      TabOrder = 2
      OnClick = btnRefreshRecordClick
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 85
      Top = 4
      Width = 75
      Height = 31
      Align = alLeft
      Caption = 'Close'
      TabOrder = 3
      OnClick = btnCloseClick
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 39
    Width = 669
    Height = 352
    Align = alClient
    DataSource = dsrcArticles
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
        Title.Caption = '#ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'code'
        Title.Caption = 'Code'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'description'
        Title.Caption = 'Description'
        Width = 265
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'price'
        Title.Caption = 'Price'
        Visible = True
      end>
  end
  object dsArticles: TFDMemTable
    AfterOpen = dsArticlesAfterOpen
    BeforePost = dsArticlesBeforePost
    BeforeDelete = dsArticlesBeforeDelete
    BeforeRefresh = dsArticlesBeforeRefresh
    FieldDefs = <>
    IndexDefs = <>
    BeforeRowRequest = dsArticlesBeforeRowRequest
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    StoreDefs = True
    Left = 136
    Top = 120
    object dsArticlesid: TIntegerField
      FieldName = 'id'
    end
    object dsArticlescode: TStringField
      FieldName = 'code'
    end
    object dsArticlesdescription: TStringField
      FieldName = 'description'
      Size = 50
    end
    object dsArticlesprice: TCurrencyField
      FieldName = 'price'
    end
  end
  object dsrcArticles: TDataSource
    DataSet = dsArticles
    Left = 136
    Top = 184
  end
end
