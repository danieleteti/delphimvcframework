require "lua_taival.delphiobjects"
require "logging.file"
logger = logging.file("test_05_TEST_%s.log", "%Y-%m-%d")

local sl = connect_to_delphi_object(stringlist);
sl.Text = "Daniele Teti"
logger:debug(sl.Text);
