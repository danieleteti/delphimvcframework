object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Server Sent Event - Viewer'
  ClientHeight = 380
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 0
    Top = 0
    Width = 802
    Height = 81
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object mmMessages: TMemo
    Left = 0
    Top = 81
    Width = 802
    Height = 299
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 0
    ExplicitWidth = 687
    ExplicitHeight = 305
  end
end
