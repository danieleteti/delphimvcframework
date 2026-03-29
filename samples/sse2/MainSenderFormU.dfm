object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Server Sent Events 2 Sender'
  ClientHeight = 100
  ClientWidth = 523
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
    Left = 8
    Top = 21
    Width = 81
    Height = 13
    Caption = 'Message to send'
  end
  object edtMessage: TEdit
    Left = 8
    Top = 40
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object btnSend: TButton
    Left = 247
    Top = 38
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object HTTPSend: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    AllowCookies = True
    HandleRedirects = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 352
    Top = 32
  end
end
