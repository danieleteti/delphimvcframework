object WaitingForm: TWaitingForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 149
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  TextHeight = 15
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 616
    Height = 149
    Align = alClient
    Pen.Color = clSilver
    Pen.Style = psInsideFrame
    ExplicitLeft = 256
    ExplicitTop = 56
    ExplicitWidth = 65
    ExplicitHeight = 65
  end
  object lblMessage: TLabel
    Left = 0
    Top = 0
    Width = 616
    Height = 149
    Align = alClient
    Alignment = taCenter
    Caption = 'Please wait'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -32
    Font.Name = 'Segoe UI Light'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 148
    ExplicitHeight = 45
  end
  object lblRunningRequests: TLabel
    Left = 8
    Top = 126
    Width = 92
    Height = 15
    Caption = 'Running requests'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object TimerWaiting: TTimer
    Enabled = False
    Interval = 900
    OnTimer = TimerWaitingTimer
    Left = 56
    Top = 40
  end
end
