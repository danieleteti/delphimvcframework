object MyDataModule: TMyDataModule
  OldCreateOrder = False
  Height = 308
  Width = 560
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=employee'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'DriverID=FB')
    ConnectedStoredUsage = [auDesignTime]
    Connected = True
    LoginPrompt = False
    Left = 160
    Top = 40
  end
  object qryCustomers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from customer')
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
end
