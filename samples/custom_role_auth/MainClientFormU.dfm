object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 434
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    556
    434)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 31
    Width = 89
    Height = 13
    Caption = 'Available accounts'
  end
  object Label4: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 14
    Caption = 'STEP 1'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 145
    Top = 8
    Width = 42
    Height = 14
    Caption = 'STEP 2'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Label6: TLabel
    Left = 312
    Top = 8
    Width = 42
    Height = 14
    Caption = 'STEP 3'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Label7: TLabel
    Left = 8
    Top = 129
    Width = 119
    Height = 52
    Caption = 
      'user1 has "role1", user2 has "role2", user3 has "role1 and role2' +
      '", admin has all the roles'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 145
    Top = 31
    Width = 161
    Height = 147
    Caption = 'Login/Logout'
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 20
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 16
      Top = 66
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object edtUsername: TEdit
      Left = 16
      Top = 39
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'user1'
    end
    object edtPassword: TEdit
      Left = 16
      Top = 85
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'user1pass'
    end
    object btnLogInLogOut: TButton
      Left = 16
      Top = 112
      Width = 121
      Height = 25
      Caption = 'btnLogInLogOut'
      TabOrder = 2
      OnClick = btnLogInLogOutClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 407
    Width = 556
    Height = 27
    Align = alBottom
    TabOrder = 10
  end
  object Button1: TButton
    Left = 311
    Top = 31
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call Public Action (no login required)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 262
    Width = 540
    Height = 139
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 9
  end
  object Button2: TButton
    Left = 311
    Top = 97
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call action allowed only for "role1"'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 311
    Top = 130
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call action allowed only for "role2"'
    TabOrder = 5
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 51
    Width = 131
    Height = 72
    ItemHeight = 13
    Items.Strings = (
      'user1:user1pass'
      'user2:user2pass'
      'user1_2:user1_2pass'
      'user3:user3pass'
      'admin:adminpass')
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object Button4: TButton
    Left = 311
    Top = 163
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call action allowed only for "role1 and role2"'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 311
    Top = 229
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call dynamic eval for admin'
    TabOrder = 8
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 311
    Top = 64
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call Authenticated only'
    TabOrder = 1
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 311
    Top = 196
    Width = 237
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call action allowed only for "role1 or role2"'
    TabOrder = 7
    OnClick = Button7Click
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 16
    Top = 184
  end
end
