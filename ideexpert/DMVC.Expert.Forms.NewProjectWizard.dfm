object frmDMVCNewProject: TfrmDMVCNewProject
  Left = 0
  Top = 0
  Caption = 'New DUnitX Project Wizard'
  ClientHeight = 311
  ClientWidth = 284
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
    284
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object lblWbModule: TLabel
    Left = 24
    Top = 45
    Width = 114
    Height = 13
    Caption = 'WebModule Class Name'
  end
  object gbControllerUnitOptions: TGroupBox
    Left = 8
    Top = 144
    Width = 268
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Controller Unit Options'
    TabOrder = 2
    DesignSize = (
      268
      121)
    object lblClassName: TLabel
      Left = 16
      Top = 54
      Width = 105
      Height = 13
      Caption = 'Controller Class Name'
    end
    object Label1: TLabel
      Left = 16
      Top = -48
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object chkCreateIndexMethod: TCheckBox
      Left = 16
      Top = 31
      Width = 236
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Index Method'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object edtClassName: TEdit
      Left = 16
      Top = 73
      Width = 236
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 120
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 201
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object chkCreateControllerUnit: TCheckBox
    Left = 24
    Top = 107
    Width = 217
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create Controller Unit'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chkCreateControllerUnitClick
  end
  object chkAddToProjectGroup: TCheckBox
    Left = 24
    Top = 10
    Width = 268
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add to Existing Project Group'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object edtWebModuleName: TEdit
    Left = 24
    Top = 64
    Width = 236
    Height = 21
    TabOrder = 5
  end
end
