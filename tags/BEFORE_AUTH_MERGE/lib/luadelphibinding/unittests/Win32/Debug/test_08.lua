require "logging.file"
--logger = logging.file("test_08_TEST_%s.log", "%Y-%m-%d")
logger = logging.file("test_08_TEST.log")
logger:debug("running the file...")

function myfunc(a,b,c)
	return a..b..c
end
