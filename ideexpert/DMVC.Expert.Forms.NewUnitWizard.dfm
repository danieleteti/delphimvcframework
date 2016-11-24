object frmDMVCNewUnit: TfrmDMVCNewUnit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New DMVC Controller Unit Wizard'
  ClientHeight = 209
  ClientWidth = 271
  Color = clBtnFace
  Constraints.MinHeight = 145
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    271
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 255
    Height = 152
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Controller Unit Options'
    TabOrder = 0
    ExplicitWidth = 246
    DesignSize = (
      255
      152)
    object lblClassName: TLabel
      Left = 16
      Top = 94
      Width = 105
      Height = 13
      Caption = 'Controller Class Name'
    end
    object edtClassName: TEdit
      Left = 14
      Top = 113
      Width = 229
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 220
    end
    object chkCreateIndexMethod: TCheckBox
      Left = 16
      Top = 32
      Width = 227
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Index Method'
      Checked = True
      State = cbChecked
      TabOrder = 0
      ExplicitWidth = 218
    end
    object chkCreateActionFiltersMethods: TCheckBox
      Left = 16
      Top = 55
      Width = 245
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Action Filters Methods'
      Checked = True
      State = cbChecked
      TabOrder = 2
      ExplicitWidth = 236
    end
  end
  object btnOK: TButton
    Left = 107
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 98
    ExplicitTop = 166
  end
  object btnCancel: TButton
    Left = 188
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 179
    ExplicitTop = 166
  end
end
