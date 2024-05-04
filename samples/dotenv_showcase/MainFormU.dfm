object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'dotEnv :: ShowCase'
  ClientHeight = 634
  ClientWidth = 1178
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1172
    Height = 628
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1168
    ExplicitHeight = 627
    object TabSheet1: TTabSheet
      Caption = 'dotEnv Samples'
      DesignSize = (
        1164
        598)
      object btnSingleEnv: TButton
        Left = 8
        Top = 236
        Width = 169
        Height = 49
        Caption = 'Single ENV without inheritance (only prod)'
        TabOrder = 0
        WordWrap = True
        OnClick = btnSingleEnvClick
      end
      object btnRequireKeys: TButton
        Left = 8
        Top = 328
        Width = 169
        Height = 49
        Caption = 'Require Keys (OK)'
        TabOrder = 1
        OnClick = btnRequireKeysClick
      end
      object btnRequireKeys2: TButton
        Left = 8
        Top = 383
        Width = 169
        Height = 49
        Caption = 'Require Keys (FAIL)'
        TabOrder = 2
        OnClick = btnRequireKeys2Click
      end
      object btnProdEnv: TButton
        Left = 8
        Top = 126
        Width = 169
        Height = 49
        Caption = 'Prod ENV (default + prod)'
        TabOrder = 3
        OnClick = btnProdEnvClick
      end
      object btnTestEnv: TButton
        Left = 8
        Top = 71
        Width = 169
        Height = 49
        Caption = 'Test ENV (default + test)'
        TabOrder = 4
        OnClick = btnTestEnvClick
      end
      object btnSimple: TButton
        Left = 8
        Top = 16
        Width = 169
        Height = 49
        Caption = 'default ENV'
        TabOrder = 5
        OnClick = btnSimpleClick
      end
      object mmVars: TMemo
        Left = 183
        Top = 18
        Width = 982
        Height = 588
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        ExplicitWidth = 978
        ExplicitHeight = 587
      end
      object btnSkipDefaultFile: TButton
        Left = 8
        Top = 181
        Width = 169
        Height = 49
        Caption = 'Skip Default .env file (load only .env.prod profile)'
        TabOrder = 7
        WordWrap = True
        OnClick = btnSkipDefaultFileClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'dotEnv Playground'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 625
        Top = 0
        Height = 598
        ExplicitHeight = 605
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 625
        Height = 598
        Align = alLeft
        Caption = 'Panel1'
        TabOrder = 0
        ExplicitHeight = 597
        object Label1: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 617
          Height = 25
          Align = alTop
          Caption = '.env file contents'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 142
        end
        object memSrc: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 35
          Width = 617
          Height = 559
          Align = alClient
          Ctl3D = True
          EditMargins.Auto = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Consolas'
          Font.Style = []
          Lines.Strings = (
            '#############################################'
            '# Sample .env file'
            
              '# This is a sample file just to play around with the .env syntax' +
              '.'
            
              '# Feel free to change it and see how the variables are interpola' +
              'ted'
            '# and provided to your app at run-time'
            '#############################################'
            ''
            '# This is a comment'
            ''
            '# Let'#39's configure a database connection'
            'db_host = myserver'
            'db_port = 5432'
            'db_url = pg://${db_host}:${db_port}'
            ''
            
              '## variable names can start with ". _ a-z" and can contains "0-9' +
              ' . _ a-z"'
            ''
            'valid.varname = OK  # this is valid'
            '.also.valid = OK   # this is valid'
            '__also__valid__ = OK   # this is valid'
            '.123 = OK   # this is valid'
            '_.123 = OK   # this is valid'
            
              '# 3notvalid = KO   # this is not valid (try to uncomment and see' +
              ' the error)'
            ''
            '#############################################'
            '# Logging configuration'
            '#############################################'
            ''
            
              '# loglevel can be: debug, normal (default), warning, error, exce' +
              'ption'
            'loglevel = error'
            ''
            'dmvc.profiler.enabled = false'
            'dmvc.profiler.warning_threshold = 3000'
            ''
            '#############################################'
            '# EMail configuration'
            '#############################################'
            ''
            'email.sender = peter.parker@bittime.com'
            'email.smtpserver = mail.bittime.com'
            'email.smtpserverport = 25'
            ''
            '#############################################'
            '# FireDAC configuration'
            '#############################################'
            ''
            '# enable or disable tracing'
            'firedac.trace = true'
            ''
            '# trace file name '
            'firedac.trace_filename = firedaclog.log'
            ''
            '# append or replace the trace file contents at each execution?'
            'firedac.trace_file_append = false '
            ''
            ''
            '')
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          WordWrap = False
          OnChange = memSrcChange
        end
      end
      object Panel2: TPanel
        Left = 628
        Top = 0
        Width = 536
        Height = 598
        Align = alClient
        Caption = 'Panel2'
        TabOrder = 1
        ExplicitWidth = 532
        ExplicitHeight = 597
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 211
          Height = 25
          Align = alTop
          Caption = 'What'#39's application "sees"'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object memDst: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 35
          Width = 528
          Height = 559
          Align = alClient
          Ctl3D = True
          EditMargins.Auto = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Consolas'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
          WordWrap = False
          ExplicitWidth = 524
          ExplicitHeight = 558
        end
      end
    end
  end
end
