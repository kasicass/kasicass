--
--  Weather update client in lua
--  Connects SUB socket to tcp://localhost:5556
--  Collects weather updates and finds avg temp in zipcode
--
--  ZeJiang Tang <kasicass@gmail.com>
--

require("zmq")

local context = zmq.init(1)

-- Socket to talk to server
print("Collecting updates from weather server...")
local subscriber = context:socket(zmq.SUB)
subscriber:connect("tcp://localhost:5556")

-- Subscribe to zipcode, default is NYC, 10001
local filter = #arg > 1 and arg[1] or "10001"
subscriber:setopt(zmq.SUBSCRIBE, filter)

-- Process 100 updates
local update_nbr = 100
local total_temp = 0
for i = 0, update_nbr-1 do
    local string = subscriber:recv()
    local zipcode, temperature, relhumidity = string.gmatch(string, "(%d+) %--(%d+) %--(%d+)")()
    total_temp = total_temp + tonumber(temperature)
end

print(string.format("Average temperature for zipcode '%s' was %dF", filter, total_temp / update_nbr))

