require "lua_taival.delphiobjects"
require "logging.file"
logger = logging.file("test_07_TEST_%s.log", "%Y-%m-%d")

--this should raise an exception
local fs = create_delphi_object('System.Classes.TFileStream');
