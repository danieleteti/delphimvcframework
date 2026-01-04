object dmMain: TdmMain
  Height = 242
  Width = 476
  object Connection: TFDConnection
    Params.Strings = (
      'Database=serversentevents.db'
      'DriverID=SQLite'
      'CharacterSet=UTF8')
    LoginPrompt = False
    AfterConnect = ConnectionAfterConnect
    BeforeConnect = ConnectionBeforeConnect
    Left = 80
    Top = 56
  end
  object qryInsertNotification: TFDQuery
    Connection = Connection
    SQL.Strings = (

        'INSERT INTO notifications (VALUE, CREATED_AT) VALUES (:STATE, da' +
        'tetime('#39'now'#39'))')
    Left = 208
    Top = 56
    ParamData = <
      item
        Name = 'STATE'
        DataType = ftWideString
        ParamType = ptInput
        Value = Null
      end>
  end
end
