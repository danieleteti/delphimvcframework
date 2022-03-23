require "lua_taival.delphiobjects"
require "logging.file"

logger = logging.file("test_03_TEST.log")

local s = connect_to_delphi_object(person);
s.FirstName = "Daniele";
s.LastName = "Teti";
s.Age = 30;



s.BecomeOlder(2);
first_name = s.FirstName;
last_name = s.LastName;
full_name = s.GetFullName('Mr.');
age = s.Age;


--reading property
local name = s.LastName

--writing a property
s.LastName = "Teti" --you can write to a property in this way


