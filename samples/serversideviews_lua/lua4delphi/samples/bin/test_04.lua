require "lua_taival.delphiobjects"
require "logging.file"
logger = logging.file("test_04_TEST_%s.log", "%Y-%m-%d")

local sl = connect_to_delphi_object(stringlist);

sl.Add("two");
sl.Add("three");
sl.Add("four");
logger:debug(sl.Text);
