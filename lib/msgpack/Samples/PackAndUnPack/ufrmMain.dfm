object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 341
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnTester: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btnTester'
    TabOrder = 0
    OnClick = btnTesterClick
  end
  object edtData: TEdit
    Left = 112
    Top = 18
    Width = 425
    Height = 21
    TabOrder = 1
    Text = 'msgPack?1f123Abcd<FE>?FM?/></f>F/></></f?>'
  end
  object mmoOutPut: TMemo
    Left = 8
    Top = 72
    Width = 529
    Height = 249
    TabOrder = 2
  end
  object btnDelete: TButton
    Left = 568
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btnDelete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object Button1: TButton
    Left = 568
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 568
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 568
    Top = 151
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 6
    OnClick = Button3Click
  end
  object btnCheckInteger: TButton
    Left = 568
    Top = 182
    Width = 105
    Height = 25
    Caption = 'btnCheckInteger'
    TabOrder = 7
    OnClick = btnCheckIntegerClick
  end
  object Button4: TButton
    Left = 568
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 8
    OnClick = Button4Click
  end
  object btnFile: TButton
    Left = 112
    Top = 41
    Width = 75
    Height = 25
    Caption = 'btnFile'
    TabOrder = 9
    OnClick = btnFileClick
  end
  object Button5: TButton
    Left = 568
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 10
    OnClick = Button5Click
  end
  object btnDMsgPacker: TButton
    Left = 248
    Top = 41
    Width = 113
    Height = 25
    Caption = 'btnDMsgPackHelper'
    TabOrder = 11
    OnClick = btnDMsgPackerClick
  end
  object btnCheckInt2: TButton
    Left = 568
    Top = 213
    Width = 105
    Height = 25
    Caption = 'btnCheckInteger2'
    TabOrder = 12
    OnClick = btnCheckInt2Click
  end
end
