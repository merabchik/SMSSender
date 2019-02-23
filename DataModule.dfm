object DM: TDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 387
  Width = 580
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=E:\Dropbox\projects\SMSSender\db.sqlite3'
      'Encrypt=aes-128'
      'DriverID=SQLite')
    FormatOptions.AssignedValues = [fvDefaultParamDataType]
    FormatOptions.DefaultParamDataType = ftString
    LoginPrompt = False
    Left = 88
    Top = 40
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 88
    Top = 96
  end
  object FDTableNumbers: TFDTable
    IndexFieldNames = 'id'
    Connection = FDConnection
    UpdateOptions.UpdateTableName = 'numbers'
    TableName = 'numbers'
    Left = 272
    Top = 176
    object FDTableNumbersid: TFDAutoIncField
      FieldName = 'id'
      Origin = 'id'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object FDTableNumberstype_id: TIntegerField
      FieldName = 'type_id'
      Origin = 'type_id'
    end
    object FDTableNumbersnumber: TIntegerField
      FieldName = 'number'
      Origin = 'number'
    end
    object FDTableNumberssent_cnt: TIntegerField
      FieldName = 'sent_cnt'
      Origin = 'sent_cnt'
    end
  end
  object FDQueryCustom: TFDQuery
    Connection = FDConnection
    Left = 280
    Top = 240
  end
end
