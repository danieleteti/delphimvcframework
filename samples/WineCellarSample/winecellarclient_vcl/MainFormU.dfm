object Form5: TForm5
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'DMVCFramework RESTClient'
  ClientHeight = 415
  ClientWidth = 927
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    927
    415)
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 33
    Caption = 'WineList'
    TabOrder = 0
    OnClick = Button1Click
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 47
    Width = 911
    Height = 360
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Wines'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 903
        Height = 332
        Align = alClient
        DataSource = DataSource1
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = DBGrid1DblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'id'
            Width = 42
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'name'
            Width = 183
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'year'
            Width = 105
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'grapes'
            Width = 117
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'country'
            Width = 145
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'region'
            Width = 200
            Visible = True
          end>
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Wine Edit'
      ImageIndex = 2
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 26
        Height = 13
        Caption = 'name'
        FocusControl = DBEdit1
      end
      object Label2: TLabel
        Left = 16
        Top = 56
        Width = 22
        Height = 13
        Caption = 'year'
        FocusControl = DBEdit2
      end
      object Label3: TLabel
        Left = 16
        Top = 96
        Width = 33
        Height = 13
        Caption = 'grapes'
        FocusControl = DBEdit3
      end
      object Label4: TLabel
        Left = 286
        Top = 56
        Width = 37
        Height = 13
        Caption = 'country'
        FocusControl = DBEdit4
      end
      object Label5: TLabel
        Left = 286
        Top = 96
        Width = 30
        Height = 13
        Caption = 'region'
        FocusControl = DBEdit5
      end
      object Label6: TLabel
        Left = 16
        Top = 160
        Width = 52
        Height = 13
        Caption = 'description'
        FocusControl = DBMemo1
      end
      object DBEdit1: TDBEdit
        Left = 16
        Top = 32
        Width = 570
        Height = 21
        DataField = 'name'
        DataSource = DataSource1
        TabOrder = 0
      end
      object DBEdit2: TDBEdit
        Left = 16
        Top = 72
        Width = 264
        Height = 21
        DataField = 'year'
        DataSource = DataSource1
        TabOrder = 1
      end
      object DBEdit3: TDBEdit
        Left = 16
        Top = 112
        Width = 264
        Height = 21
        DataField = 'grapes'
        DataSource = DataSource1
        TabOrder = 2
      end
      object DBEdit4: TDBEdit
        Left = 286
        Top = 72
        Width = 300
        Height = 21
        DataField = 'country'
        DataSource = DataSource1
        TabOrder = 3
      end
      object DBEdit5: TDBEdit
        Left = 286
        Top = 112
        Width = 300
        Height = 21
        DataField = 'region'
        DataSource = DataSource1
        TabOrder = 4
      end
      object DBMemo1: TDBMemo
        Left = 16
        Top = 176
        Width = 264
        Height = 153
        DataField = 'description'
        DataSource = DataSource1
        TabOrder = 5
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Raw'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 903
        Height = 332
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        ExplicitWidth = 734
        ExplicitHeight = 342
      end
    end
  end
  object DBNavigator1: TDBNavigator
    Left = 135
    Top = 8
    Width = 320
    Height = 33
    DataSource = DataSource1
    TabOrder = 1
  end
  object FDMemTable1: TFDMemTable
    BeforePost = FDMemTable1BeforePost
    BeforeDelete = FDMemTable1BeforeDelete
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 232
    Top = 184
    object FDMemTable1id: TIntegerField
      FieldName = 'id'
    end
    object FDMemTable1name: TStringField
      FieldName = 'name'
      Size = 50
    end
    object FDMemTable1year: TStringField
      FieldName = 'year'
    end
    object FDMemTable1grapes: TStringField
      FieldName = 'grapes'
      Size = 100
    end
    object FDMemTable1country: TStringField
      FieldName = 'country'
      Size = 50
    end
    object FDMemTable1region: TStringField
      FieldName = 'region'
      Size = 50
    end
    object FDMemTable1description: TMemoField
      FieldName = 'description'
      BlobType = ftMemo
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 336
    Top = 184
  end
end
