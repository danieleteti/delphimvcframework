object dmMain: TdmMain
  OldCreateOrder = False
  Height = 214
  Width = 438
  object Connection: TFDConnection
    Params.Strings = (
      'Database=C:\DEV\DMVCFramework\samples\data\ORDERSMANAGER.FDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=FB')
    ConnectedStoredUsage = []
    Connected = True
    LoginPrompt = False
    BeforeConnect = ConnectionBeforeConnect
    Left = 64
    Top = 48
  end
  object dsArticles: TFDQuery
    Active = True
    Connection = Connection
    UpdateOptions.AssignedValues = [uvFetchGeneratorsPoint, uvGeneratorName]
    UpdateOptions.FetchGeneratorsPoint = gpImmediate
    UpdateOptions.GeneratorName = 'GEN_ARTICOLI_ID'
    UpdateOptions.UpdateTableName = 'ARTICOLI'
    UpdateOptions.KeyFields = 'ID'
    UpdateObject = updArticles
    SQL.Strings = (
      'SELECT * FROM ARTICOLI')
    Left = 144
    Top = 48
  end
  object updArticles: TFDUpdateSQL
    Connection = Connection
    InsertSQL.Strings = (
      'INSERT INTO ARTICOLI'
      '(ID, CODICE, DESCRIZIONE, PREZZO)'
      'VALUES (:NEW_ID, :NEW_CODICE, :NEW_DESCRIZIONE, :NEW_PREZZO)'
      'RETURNING ID, CODICE, DESCRIZIONE, PREZZO')
    ModifySQL.Strings = (
      'UPDATE ARTICOLI'
      'SET CODICE = :NEW_CODICE, DESCRIZIONE = :NEW_DESCRIZIONE, '
      '  PREZZO = :NEW_PREZZO'
      'WHERE ID = :OLD_ID'
      'RETURNING ID, CODICE, DESCRIZIONE, PREZZO')
    DeleteSQL.Strings = (
      'DELETE FROM ARTICOLI'
      'WHERE ID = :OLD_ID')
    FetchRowSQL.Strings = (
      'SELECT ID, CODICE, DESCRIZIONE, PREZZO'
      'FROM ARTICOLI'
      'WHERE ID = :ID')
    Left = 144
    Top = 112
  end
end
