object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DMVCFramework server in dll'
  ClientHeight = 161
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 264
    Top = 96
    Width = 120
    Height = 13
    Caption = 'http://localhost:8080/api'
    OnClick = Label2Click
    OnMouseEnter = Label2MouseEnter
    OnMouseLeave = Label2MouseLeave
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 120
    Height = 13
    Caption = 'http://localhost:8090/api'
    OnClick = Label2Click
    OnMouseEnter = Label2MouseEnter
    OnMouseLeave = Label2MouseLeave
  end
  object btnStartInProcess: TButton
    Left = 8
    Top = 8
    Width = 193
    Height = 25
    Caption = 'Run in-process server (port 8090)'
    TabOrder = 0
    OnClick = btnStartInProcessClick
  end
  object Button3: TButton
    Left = 264
    Top = 8
    Width = 153
    Height = 25
    Caption = 'Run DLL Server (Port 8080)'
    TabOrder = 1
    OnClick = Button3Click
  end
  object btnStopInProcess: TButton
    Left = 8
    Top = 48
    Width = 193
    Height = 25
    Caption = 'Stop in-process server'
    TabOrder = 2
    OnClick = btnStopInProcessClick
  end
  object Button2: TButton
    Left = 264
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Stop DLL Server'
    TabOrder = 3
    OnClick = Button2Click
  end
end
