object WebModule1: TWebModule1
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <>
  Height = 344
  Width = 435
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\Public\Documents\RAD Studio\12.0\Samples\Data\' +
        'EMPLOYEE.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
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
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 160
    Top = 176
  end
end
