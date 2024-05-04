object dmMain: TdmMain
  Height = 366
  Width = 644
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=MSDASQL.1;Persist Security Info=False;Data Source=mypos' +
      'tgres;Initial Catalog=activerecorddb'
    Provider = 'MSDASQL.1'
    Left = 304
    Top = 168
  end
  object ADOQuery1: TADOQuery
    Connection = ADOConnection1
    Parameters = <>
    Left = 440
    Top = 168
  end
end
