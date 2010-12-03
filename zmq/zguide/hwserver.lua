--
-- Hello World server in lua
-- Binds REP socket to tcp://*:5555
-- Expects "Hello" from client, replies with "World"
--
-- ZeJiang Tang <kasicass@gmail.com>
--

require("zmq")

local context = zmq.init(1)
local socket  = context:socket(zmq.REP)
socket:bind("tcp://*:5555")

while true do
    -- Wait for next request from client
    local request = socket:recv()
    print(string.format("Received request: [%s]", request))
    
    -- Do some 'work'
    -- Well, lua has no native support of sleep(1)

    -- Send reply back to client
    socket:send("World")
end

