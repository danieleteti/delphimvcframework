object CustomersDM: TCustomersDM
  OldCreateOrder = False
  Height = 266
  Width = 405
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=activerecorddb'
      'User_Name=postgres'
      'Password=postgres'
      'Server=127.0.0.1'
      'DriverID=PG')
    ConnectedStoredUsage = []
    Left = 88
    Top = 64
  end
  object MyQuery: TFDQuery
    Connection = FDConnection1
    Left = 208
    Top = 64
  end
end
