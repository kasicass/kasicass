--
--  Report 0MQ version
--
--  ZeJiang Tang <kasicass@gmail.com>
--

require("zmq")
local major, minor, patch = unpack(zmq.version())
print(major, minor, patch)

