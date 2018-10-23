object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LoggerPro SAMPLE'
  ClientHeight = 535
  ClientWidth = 834
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  DesignSize = (
    834
    535)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 57
    Caption = 'DEBUG'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 151
    Top = 8
    Width = 137
    Height = 57
    Caption = 'INFO'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 294
    Top = 8
    Width = 137
    Height = 57
    Caption = 'WARNING'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 437
    Top = 8
    Width = 137
    Height = 57
    Caption = 'ERROR'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 71
    Width = 280
    Height = 57
    Caption = 'Multithread logging'
    TabOrder = 4
    OnClick = Button5Click
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 134
    Width = 818
    Height = 393
    ActivePage = tsListViewAppender
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    object tsListViewAppender: TTabSheet
      Caption = 'ListView Appender DEMO'
      object ListView1: TListView
        Left = 0
        Top = 0
        Width = 810
        Height = 365
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Application Logs'
          end>
        ColumnClick = False
        Ctl3D = False
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        OwnerDraw = True
        RowSelect = True
        ParentDoubleBuffered = False
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsMemoAppender: TTabSheet
      Caption = 'Memo Appender DEMO'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 810
        Height = 365
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
        ExplicitLeft = -535
        ExplicitTop = -228
        ExplicitWidth = 816
        ExplicitHeight = 393
      end
    end
  end
end
