object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Articles CRUD SAMPLE'
  ClientHeight = 391
  ClientWidth = 876
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 876
    Height = 112
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 872
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 585
      Top = 4
      Width = 287
      Height = 40
      DataSource = dsrcArticles
      Align = alRight
      TabOrder = 3
      ExplicitLeft = 581
    end
    object btnOpen: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 75
      Height = 40
      Align = alLeft
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnRefreshRecord: TButton
      AlignWithMargins = True
      Left = 166
      Top = 4
      Width = 107
      Height = 40
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
      Height = 40
      Align = alLeft
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCloseClick
    end
    object Panel2: TPanel
      Left = 1
      Top = 47
      Width = 874
      Height = 64
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitWidth = 870
      object Label1: TLabel
        Left = 3
        Top = 11
        Width = 24
        Height = 13
        Caption = 'Filter'
      end
      object EditFilter: TEdit
        Left = 3
        Top = 30
        Width = 156
        Height = 21
        TabOrder = 1
      end
      object btnFilter: TButton
        Left = 165
        Top = 19
        Width = 107
        Height = 40
        Caption = 'Filter by description'
        TabOrder = 0
        OnClick = btnFilterClick
      end
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 112
    Width = 876
    Height = 279
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
      end
      item
        Expanded = False
        FieldName = 'createdat'
        ReadOnly = True
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'updatedat'
        ReadOnly = True
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
    object dsArticlescreatedat: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'createdat'
    end
    object dsArticlesupdatedat: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'updatedat'
    end
  end
  object dsrcArticles: TDataSource
    DataSet = dsArticles
    Left = 136
    Top = 184
  end
end
