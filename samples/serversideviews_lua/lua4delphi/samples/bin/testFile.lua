require "logging.file"
local logger = logging.file("__TEST%s.log", "%Y-%m-%d")
logger:info("logging.file test")
logger:debug("debugging...")
logger:error("error!")
logger:info({id = "1"})
