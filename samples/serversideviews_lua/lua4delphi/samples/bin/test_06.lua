require "lua_taival.delphiobjects"
require "logging.file"
logger = logging.file("test_06_TEST.log")

local sl = create_delphi_object('System.Classes.TStringList')
sl.Text = "Daniele Teti"
logger:debug('Ecco il contenuto della TStringList: '..sl.Text);
sl.Free();
