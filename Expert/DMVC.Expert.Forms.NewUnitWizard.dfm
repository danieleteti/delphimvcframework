object frmDMVCNewUnit: TfrmDMVCNewUnit
  Left = 0
  Top = 0
  Caption = 'New DMVC Controller Unit Wizard'
  ClientHeight = 163
  ClientWidth = 262
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
    262
    163)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 246
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Controller Unit Options'
    TabOrder = 0
    DesignSize = (
      246
      113)
    object lblClassName: TLabel
      Left = 16
      Top = 70
      Width = 105
      Height = 13
      Caption = 'Controller Class Name'
    end
    object edtClassName: TEdit
      Left = 16
      Top = 89
      Width = 220
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object chkCreateIndexMethod: TCheckBox
      Left = 16
      Top = 32
      Width = 218
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Index Method'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 98
    Top = 130
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 179
    Top = 130
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
