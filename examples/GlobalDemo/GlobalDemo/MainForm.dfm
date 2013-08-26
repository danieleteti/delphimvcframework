object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Sender Form'
  ClientHeight = 340
  ClientWidth = 651
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
  object pnlConnection: TPanel
    Left = 8
    Top = 8
    Width = 633
    Height = 57
    TabOrder = 0
    object Button1: TButton
      Left = 441
      Top = 17
      Width = 75
      Height = 25
      Action = acConnect
      TabOrder = 0
    end
    object edtUserName: TLabeledEdit
      Left = 187
      Top = 19
      Width = 121
      Height = 21
      EditLabel.Width = 49
      EditLabel.Height = 13
      EditLabel.Caption = 'UserName'
      TabOrder = 1
      Text = 'admin'
    end
    object edtPassword: TLabeledEdit
      Left = 314
      Top = 19
      Width = 121
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      TabOrder = 2
      Text = 'password'
    end
    object edtHostNameAndPort: TLabeledEdit
      Left = 8
      Top = 19
      Width = 173
      Height = 21
      EditLabel.Width = 72
      EditLabel.Height = 13
      EditLabel.Caption = 'Hostname:port'
      TabOrder = 3
      Text = 'localhost:61613'
    end
    object Button5: TButton
      Left = 522
      Top = 17
      Width = 75
      Height = 25
      Action = acDisconnect
      TabOrder = 4
    end
  end
  object pnlMain: TPanel
    Left = 8
    Top = 80
    Width = 633
    Height = 249
    Caption = 'pnlMain'
    Enabled = False
    TabOrder = 1
    object Button2: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Begin Trans'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 89
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Abort Trans'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 170
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Commit Trans'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button6: TButton
      Left = 354
      Top = 72
      Width = 111
      Height = 69
      Caption = 'Send'
      TabOrder = 3
      OnClick = Button6Click
    end
    object chkPersistent: TCheckBox
      Left = 251
      Top = 20
      Width = 97
      Height = 17
      Caption = 'Persistent'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object Edit1: TEdit
      Left = 8
      Top = 45
      Width = 193
      Height = 21
      TabOrder = 5
      Text = '/topic/mytopic'
      TextHint = 'Topic or queue name (/topic/thename or /queue/thename)'
    end
    object Memo1: TMemo
      Left = 8
      Top = 72
      Width = 340
      Height = 161
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 288
    Top = 264
    object acConnect: TAction
      Caption = 'Connect'
      OnExecute = acConnectExecute
    end
    object acDisconnect: TAction
      Caption = 'Disconnect'
      OnExecute = acDisconnectExecute
    end
  end
end
