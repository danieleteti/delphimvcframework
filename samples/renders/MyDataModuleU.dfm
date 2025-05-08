object MyDataModule: TMyDataModule
  Height = 308
  Width = 560
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=employee'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'CharacterSet=UTF8'
      'DriverID=FB')
    ConnectedStoredUsage = []
    Connected = True
    LoginPrompt = False
    Left = 160
    Top = 40
  end
  object qryCustomers: TFDQuery
    Connection = FDConnection1
    FetchOptions.AssignedValues = [evUnidirectional]
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    SQL.Strings = (
      
        'select customer.*, '#39#20320#22909#39' as cn_hello, '#39#1047#1076#1088#1072#1074#1086#39' as sr_hello from c' +
        'ustomer')
    Left = 160
    Top = 112
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 160
    Top = 200
  end
  object qryCountry: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM COUNTRY ORDER BY COUNTRY')
    Left = 256
    Top = 112
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 264
    Top = 200
  end
end
