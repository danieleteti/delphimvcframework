object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sqids Showcase Sample'
  ClientHeight = 441
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 21
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 62
    Height = 21
    Caption = 'Alphabet'
  end
  object Label2: TLabel
    Left = 8
    Top = 77
    Width = 79
    Height = 21
    Caption = 'Min Lenght'
  end
  object lblMinSize: TLabel
    Left = 463
    Top = 104
    Width = 74
    Height = 32
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object EditAlphabet: TEdit
    Left = 8
    Top = 40
    Width = 449
    Height = 29
    TabOrder = 0
  end
  object TrackBarMinLength: TTrackBar
    Left = 8
    Top = 104
    Width = 449
    Height = 45
    Max = 20
    TabOrder = 1
    OnChange = TrackBarMinLengthChange
  end
  object btnShuffle: TButton
    Left = 462
    Top = 40
    Width = 75
    Height = 29
    Caption = 'Shuffle'
    TabOrder = 2
    OnClick = btnShuffleClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 168
    Width = 530
    Height = 265
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Encode'
      object Label3: TLabel
        Left = 16
        Top = 13
        Width = 345
        Height = 21
        Caption = 'Number or numbers (comma separated e.g. 1,2,3)'
      end
      object Label4: TLabel
        Left = 16
        Top = 125
        Width = 101
        Height = 21
        Caption = 'Encoded Sqids'
      end
      object EditNumbers: TEdit
        Left = 16
        Top = 40
        Width = 481
        Height = 29
        TabOrder = 0
        Text = '1,2,3'
      end
      object btnEncode: TButton
        Left = 16
        Top = 82
        Width = 180
        Height = 30
        Caption = 'Encode to Sqids'
        TabOrder = 1
        OnClick = btnEncodeClick
      end
      object EditSqidsOutput: TEdit
        Left = 16
        Top = 152
        Width = 481
        Height = 29
        ReadOnly = True
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Decode'
      ImageIndex = 1
      object Label5: TLabel
        Left = 16
        Top = 134
        Width = 205
        Height = 21
        Caption = 'Decoded number or numbers'
      end
      object Label6: TLabel
        Left = 16
        Top = 13
        Width = 101
        Height = 21
        Caption = 'Encoded Sqids'
      end
      object EditIntegersOutput: TEdit
        Left = 16
        Top = 161
        Width = 481
        Height = 29
        ReadOnly = True
        TabOrder = 0
      end
      object btnDecode: TButton
        Left = 16
        Top = 82
        Width = 180
        Height = 30
        Caption = 'Decode to Integers'
        TabOrder = 1
        OnClick = btnDecodeClick
      end
      object EditSqidsInput: TEdit
        Left = 16
        Top = 40
        Width = 481
        Height = 29
        TabOrder = 2
      end
    end
  end
end
