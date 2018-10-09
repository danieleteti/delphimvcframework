object EntitiesModule: TEntitiesModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 241
  Width = 717
  object Entity: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 136
    Top = 24
    object EntityId: TLargeintField
      FieldName = 'Id'
    end
    object EntityCode: TIntegerField
      FieldName = 'Code'
    end
    object EntityName: TStringField
      FieldName = 'Name'
      Size = 60
    end
    object EntitySalary: TCurrencyField
      FieldName = 'Salary'
    end
    object EntityBirthday: TDateField
      FieldName = 'Birthday'
    end
    object EntityAccessDateTime: TDateTimeField
      FieldName = 'AccessDateTime'
    end
    object EntityAccessTime: TTimeField
      FieldName = 'AccessTime'
    end
    object EntityActive: TBooleanField
      FieldName = 'Active'
    end
    object EntityAmount: TFloatField
      FieldName = 'Amount'
    end
    object EntityIgnored: TStringField
      FieldName = 'Ignored'
      Size = 30
    end
    object EntityIgnoredAtt: TStringField
      FieldName = 'IgnoredAtt'
      Size = 30
    end
    object EntityBlobFld: TBlobField
      FieldName = 'BlobFld'
    end
    object EntityItems: TDataSetField
      FieldName = 'Items'
    end
    object EntityDepartament: TDataSetField
      FieldName = 'Departament'
    end
    object EntityGUID: TGuidField
      FieldName = 'GUID'
      Size = 38
    end
  end
  object EntityLowerCase: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 280
    Top = 56
    object EntityLowerCaseId: TLargeintField
      FieldName = 'Id'
    end
    object EntityLowerCaseName: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
  object EntityUpperCase: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 392
    Top = 56
    object EntityUpperCaseId: TLargeintField
      FieldName = 'Id'
    end
    object EntityUpperCaseName: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
  object EntityUpperCase2: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 504
    Top = 56
    object EntityUpperCase2Id: TLargeintField
      FieldName = 'Id'
    end
    object EntityUpperCase2Name: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
  object Item: TClientDataSet
    Aggregates = <>
    DataSetField = EntityItems
    Params = <>
    Left = 176
    Top = 80
    object ItemId: TLargeintField
      FieldName = 'Id'
    end
    object ItemName: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
  object Departament: TClientDataSet
    Aggregates = <>
    DataSetField = EntityDepartament
    Params = <>
    Left = 96
    Top = 80
    object DepartamentName: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
  object EntityAsIs: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 608
    Top = 56
    object EntityAsIsId: TLargeintField
      FieldName = 'Id'
    end
    object EntityAsIsName: TStringField
      FieldName = 'Name'
      Size = 60
    end
  end
end
