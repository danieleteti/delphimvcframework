object Form5: TForm5
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'DMVCFramework RESTClient'
  ClientHeight = 425
  ClientWidth = 758
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    758
    425)
  PixelsPerInch = 96
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
    Width = 742
    Height = 370
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Wines'
      object ListView1: TListView
        Left = 0
        Top = 0
        Width = 734
        Height = 342
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'name'
          end
          item
            Caption = 'year'
          end
          item
            AutoSize = True
            Caption = 'grapes'
          end
          item
            AutoSize = True
            Caption = 'country'
          end
          item
            AutoSize = True
            Caption = 'region'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = ListView1DblClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Wine Edit'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 26
        Height = 13
        Caption = 'name'
      end
      object Label2: TLabel
        Left = 16
        Top = 56
        Width = 22
        Height = 13
        Caption = 'year'
      end
      object Label3: TLabel
        Left = 16
        Top = 96
        Width = 33
        Height = 13
        Caption = 'grapes'
      end
      object Label4: TLabel
        Left = 286
        Top = 56
        Width = 37
        Height = 13
        Caption = 'country'
      end
      object Label5: TLabel
        Left = 286
        Top = 96
        Width = 30
        Height = 13
        Caption = 'region'
      end
      object Label6: TLabel
        Left = 16
        Top = 160
        Width = 52
        Height = 13
        Caption = 'description'
      end
      object Edit1: TEdit
        Left = 16
        Top = 29
        Width = 193
        Height = 21
        TabOrder = 0
      end
      object Edit2: TEdit
        Left = 16
        Top = 69
        Width = 193
        Height = 21
        TabOrder = 1
      end
      object Edit3: TEdit
        Left = 286
        Top = 69
        Width = 193
        Height = 21
        TabOrder = 2
      end
      object Edit4: TEdit
        Left = 16
        Top = 109
        Width = 193
        Height = 21
        TabOrder = 3
      end
      object Edit5: TEdit
        Left = 286
        Top = 109
        Width = 193
        Height = 21
        TabOrder = 4
      end
      object Memo2: TMemo
        Left = 16
        Top = 179
        Width = 463
        Height = 103
        TabOrder = 5
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Raw'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 734
        Height = 342
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object BindNavigator1: TBindNavigator
    Left = 135
    Top = 8
    Width = 290
    Height = 33
    DataSource = PrototypeBindSource1
    Orientation = orHorizontal
    TabOrder = 2
    BeforeAction = BindNavigator1BeforeAction
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = False
    AutoPost = True
    RecordCount = 50
    FieldDefs = <
      item
        Name = 'id'
        FieldType = ftUInteger
        Generator = 'AlphaColors'
        ReadOnly = False
      end
      item
        Name = 'name'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'year'
        FieldType = ftUInteger
        Generator = 'AlphaColors'
        ReadOnly = False
      end
      item
        Name = 'grapes'
        Generator = 'ColorsNames'
        ReadOnly = False
      end
      item
        Name = 'country'
        Generator = 'ColorsNames'
        ReadOnly = False
      end
      item
        Name = 'region'
        Generator = 'ColorsNames'
        ReadOnly = False
      end
      item
        Name = 'description'
        FieldType = ftTStrings
        Generator = 'LoremIpsum'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = PrototypeBindSource1CreateAdapter
    Left = 320
    Top = 160
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 180
    Top = 157
    object LinkListControlToField1: TLinkListControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'name'
      Control = ListView1
      FillExpressions = <
        item
          SourceMemberName = 'year'
          ControlMemberName = 'SubItems[0]'
        end
        item
          SourceMemberName = 'grapes'
          ControlMemberName = 'SubItems[1]'
        end
        item
          SourceMemberName = 'country'
          ControlMemberName = 'SubItems[2]'
        end
        item
          SourceMemberName = 'region'
          ControlMemberName = 'SubItems[3]'
        end>
      FillHeaderExpressions = <>
      FillBreakGroups = <>
    end
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'name'
      Control = Edit1
      Track = True
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'year'
      Control = Edit2
      Track = True
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'grapes'
      Control = Edit4
      Track = True
    end
    object LinkControlToField4: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'country'
      Control = Edit3
      Track = True
    end
    object LinkControlToField5: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'region'
      Control = Edit5
      Track = True
    end
    object LinkControlToField6: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'description'
      Control = Memo2
      Track = False
    end
  end
end
