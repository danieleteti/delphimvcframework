object CustomersTDG: TCustomersTDG
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=CUSTOMERSITDEVCON')
    LoginPrompt = False
    Left = 80
    Top = 48
  end
end
