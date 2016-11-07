object PeopleModule: TPeopleModule
  OldCreateOrder = True
  Height = 199
  Width = 436
  object qryPeople: TFDQuery
    Connection = Conn
    UpdateObject = updPeople
    SQL.Strings = (
      'SELECT * FROM PEOPLE')
    Left = 24
    Top = 104
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
    Left = 112
    Top = 104
  end
  object Conn: TFDConnection
    Params.Strings = (
      'Database=C:\Delphi Cookbook\BOOK\Chapter05\DATA\SAMPLES.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    Connected = True
    LoginPrompt = False
    BeforeConnect = ConnBeforeConnect
    Left = 24
    Top = 16
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 112
    Top = 16
  end
end
