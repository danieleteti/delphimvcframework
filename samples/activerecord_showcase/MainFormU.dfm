object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMVCActiveRecord - ShowCase'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCRUD: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 33
    Caption = 'CRUD'
    TabOrder = 0
    OnClick = btnCRUDClick
  end
  object btnSelect: TButton
    Left = 8
    Top = 47
    Width = 121
    Height = 33
    Caption = 'Queries'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object Memo1: TMemo
    Left = 135
    Top = 8
    Width = 492
    Height = 283
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnRelations: TButton
    Left = 8
    Top = 86
    Width = 121
    Height = 35
    Caption = 'Relations'
    TabOrder = 3
    OnClick = btnRelationsClick
  end
  object btnInheritance: TButton
    Left = 8
    Top = 127
    Width = 121
    Height = 34
    Caption = 'Inheritance'
    TabOrder = 4
    OnClick = btnInheritanceClick
  end
  object btnValidation: TButton
    Left = 8
    Top = 167
    Width = 121
    Height = 34
    Caption = 'Validation'
    TabOrder = 5
    OnClick = btnValidationClick
  end
  object btnMultiThreading: TButton
    Left = 8
    Top = 247
    Width = 121
    Height = 34
    Caption = 'Multi Threading'
    TabOrder = 6
    OnClick = btnMultiThreadingClick
  end
  object btnRQL: TButton
    Left = 8
    Top = 207
    Width = 121
    Height = 34
    Caption = 'RQL Query'
    TabOrder = 7
    OnClick = btnRQLClick
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      
        'Database=C:\DEV\dmvcframework\samples\data\ORDERSMANAGER_FB30.FD' +
        'B'
      'DriverID=FB')
    Left = 184
    Top = 40
  end
end
