object PeopleModule: TPeopleModule
  OldCreateOrder = True
  Height = 199
  Width = 258
  object qryPeople: TFDQuery
    Connection = Conn
    UpdateObject = updPeople
    SQL.Strings = (
      'SELECT * FROM PEOPLE')
    Left = 64
    Top = 120
  end
  object updPeople: TFDUpdateSQL
    Connection = Conn
    InsertSQL.Strings = (
      'INSERT INTO PEOPLE'
      '(FIRST_NAME, LAST_NAME, WORK_PHONE_NUMBER, MOBILE_PHONE_NUMBER, '
      '  EMAIL)'
      
        'VALUES (:NEW_FIRST_NAME, :NEW_LAST_NAME, :NEW_WORK_PHONE_NUMBER,' +
        ' :NEW_MOBILE_PHONE_NUMBER, '
      '  :NEW_EMAIL)')
    ModifySQL.Strings = (
      'UPDATE PEOPLE'
      
        'SET FIRST_NAME = :NEW_FIRST_NAME, LAST_NAME = :NEW_LAST_NAME, WO' +
        'RK_PHONE_NUMBER = :NEW_WORK_PHONE_NUMBER, '
      
        '  MOBILE_PHONE_NUMBER = :NEW_MOBILE_PHONE_NUMBER, EMAIL = :NEW_E' +
        'MAIL'
      'WHERE ID = :OLD_ID')
    DeleteSQL.Strings = (
      'DELETE FROM PEOPLE'
      'WHERE ID = :OLD_ID')
    FetchRowSQL.Strings = (
      
        'SELECT ID, FIRST_NAME, LAST_NAME, WORK_PHONE_NUMBER, MOBILE_PHON' +
        'E_NUMBER, '
      '  EMAIL'
      'FROM PEOPLE'
      'WHERE ID = :ID')
    Left = 152
    Top = 120
  end
  object Conn: TFDConnection
    Params.Strings = (
      
        'Database=D:\DEV\dmvcframework\samples\outputcachewithredis\bin\O' +
        'utputCacheWithRedisdb'
      'DriverID=SQLite')
    ConnectedStoredUsage = []
    LoginPrompt = False
    AfterConnect = ConnAfterConnect
    BeforeConnect = ConnBeforeConnect
    Left = 64
    Top = 32
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 152
    Top = 32
  end
end
