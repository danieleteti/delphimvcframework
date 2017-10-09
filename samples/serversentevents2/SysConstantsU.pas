unit SysConstantsU;

interface

const
  BASEURL = '/api';

  SQL_CREATE_TABLE = 'CREATE TABLE IF NOT EXISTS notifications ( ' + sLineBreak +
    'id INTEGER PRIMARY KEY, ' + sLineBreak +
    'value text NOT NULL, ' + sLineBreak +
    'created_at text NOT NULL ' + sLineBreak +
    ')';

implementation

end.
