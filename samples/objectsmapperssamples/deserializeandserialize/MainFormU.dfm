object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Deserialize nested classes'
  ClientHeight = 392
  ClientWidth = 771
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    771
    392)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 153
    Height = 25
    Caption = 'Deserialize and Serialize'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 755
    Height = 219
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '{"nestedProperty":'
      '  {'
      '    "nestedString": "bar",'
      '    "nestedInteger": 4,'
      '    "nestedFloat": 4.56,'
      '    "nestedDateTime": "2015-01-01 01:12:34"'
      '  },'
      '  "nestedList": ['
      '    {"key":"FirstName","value":"Daniele"},'
      '    {"key":"LastName","value":"Teti"},'
      '    {"key":"eMail","value":"d.teti@bittime.it"}'
      '  ],'
      '"parentString":"foo"}')
    ParentFont = False
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 264
    Width = 755
    Height = 120
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
end
