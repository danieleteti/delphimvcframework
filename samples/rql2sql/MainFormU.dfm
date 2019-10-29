object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'RQL2SQL'
  ClientHeight = 423
  ClientWidth = 897
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 121
    Height = 423
    Align = alLeft
    TabOrder = 0
    object btnParse: TButton
      Left = 8
      Top = 8
      Width = 105
      Height = 65
      Caption = 'RQL to SQL'
      TabOrder = 0
      OnClick = btnParseClick
    end
  end
  object Panel2: TPanel
    Left = 121
    Top = 0
    Width = 776
    Height = 423
    Align = alClient
    Caption = 'Panel2'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 6
      Top = 199
      Width = 764
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 62
    end
    object Splitter2: TSplitter
      Left = 6
      Top = 369
      Width = 764
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 368
    end
    object mmSQL: TMemo
      AlignWithMargins = True
      Left = 9
      Top = 205
      Width = 758
      Height = 161
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object lbRQL: TListBox
      AlignWithMargins = True
      Left = 9
      Top = 117
      Width = 758
      Height = 79
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      ItemHeight = 19
      ParentFont = False
      TabOrder = 1
      OnClick = lbRQLClick
    end
    object Panel3: TPanel
      Left = 6
      Top = 87
      Width = 764
      Height = 27
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      TabOrder = 2
      object edtExpression: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 677
        Height = 21
        Align = alClient
        TabOrder = 0
      end
      object btnAdd: TButton
        AlignWithMargins = True
        Left = 686
        Top = 3
        Width = 75
        Height = 21
        Align = alRight
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
    end
    object rgBackend: TRadioGroup
      Left = 6
      Top = 6
      Width = 764
      Height = 81
      Align = alTop
      Caption = 'Available RQL Backends'
      Columns = 2
      TabOrder = 3
    end
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 9
      Top = 376
      Width = 758
      Height = 38
      Align = alBottom
      Color = clScrollBar
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 4
    end
  end
end
