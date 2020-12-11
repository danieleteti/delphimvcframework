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
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 876
    Height = 112
    Align = alTop
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 585
      Top = 4
      Width = 287
      Height = 40
      DataSource = dsrcArticles
      Align = alRight
      TabOrder = 3
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
        Caption = 'Filter'
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
  end
  object dsrcArticles: TDataSource
    DataSet = dsArticles
    Left = 136
    Top = 184
  end
end
