object fmEsGUI: TfmEsGUI
  Left = 271
  Top = 114
  Caption = 'EEPM Server'
  ClientHeight = 194
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 283
    Height = 194
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 48
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object ButtonStart: TButton
      Left = 24
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = ButtonStartClick
    end
    object ButtonStop: TButton
      Left = 105
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = ButtonStopClick
    end
    object EditPort: TEdit
      Left = 24
      Top = 67
      Width = 121
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = '8080'
    end
    object ButtonOpenBrowser: TButton
      Left = 24
      Top = 104
      Width = 107
      Height = 25
      Caption = 'Open Browser'
      TabOrder = 3
      OnClick = ButtonOpenBrowserClick
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 168
    Top = 48
  end
end
