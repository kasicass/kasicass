--
--  Hello World client in lua
--  Connects REQ socket to tcp://localhost:5555
--  Sends "Hello" to server, expects "World" back
--
--  ZeJiang Tang <kasicass@gmail.com>
--

require("zmq")

local context = zmq.init(1)

-- Socket to talk to server
print("Connecting to hello world server...")
local requester = context:socket(zmq.REQ)
requester:connect("tcp://localhost:5555")

--  Do 10 requests, waiting each time for a response
for request_nbr = 0, 9 do
    requester:send("Hello")
    print(string.format("Sending request %d...", request_nbr))

    -- Get the reply
    local message = requester:recv()
    print(string.format("Received reply %d: [%s]", request_nbr, message))
end

