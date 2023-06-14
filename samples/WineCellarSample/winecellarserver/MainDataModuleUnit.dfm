object WineCellarDataModule: TWineCellarDataModule
  Height = 211
  Width = 336
  object Connection: TFDConnection
    Params.Strings = (
      
        'Database=C:\DEV\dmvcframework\samples\winecellarserver\WINES_FB3' +
        '0.FDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'DriverID=FB')
    ConnectedStoredUsage = [auDesignTime]
    LoginPrompt = False
    BeforeConnect = ConnectionBeforeConnect
    Left = 72
    Top = 48
  end
  object qryWines: TFDQuery
    Connection = Connection
    UpdateObject = updWines
    SQL.Strings = (
      'SELECT * FROM WINE')
    Left = 168
    Top = 48
  end
  object updWines: TFDUpdateSQL
    Connection = Connection
    InsertSQL.Strings = (
      'INSERT INTO WINE'
      '(NAME, "YEAR", GRAPES, COUNTRY, REGION, '
      '  DESCRIPTION, PICTURE)'
      
        'VALUES (:NEW_NAME, :NEW_YEAR, :NEW_GRAPES, :NEW_COUNTRY, :NEW_RE' +
        'GION, '
      '  :NEW_DESCRIPTION, :NEW_PICTURE)')
    ModifySQL.Strings = (
      'UPDATE WINE'
      'SET NAME = :NEW_NAME, "YEAR" = :NEW_YEAR, GRAPES = :NEW_GRAPES, '
      
        '  COUNTRY = :NEW_COUNTRY, REGION = :NEW_REGION, DESCRIPTION = :N' +
        'EW_DESCRIPTION, '
      '  PICTURE = :NEW_PICTURE'
      'WHERE ID = :OLD_ID')
    DeleteSQL.Strings = (
      'DELETE FROM WINE'
      'WHERE ID = :OLD_ID')
    FetchRowSQL.Strings = (
      
        'SELECT ID, NAME, "YEAR" AS "YEAR", GRAPES, COUNTRY, REGION, DESC' +
        'RIPTION, '
      '  PICTURE'
      'FROM WINE'
      'WHERE ID = :ID')
    Left = 168
    Top = 120
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 72
    Top = 120
  end
end
