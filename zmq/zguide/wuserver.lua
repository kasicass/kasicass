--
--  Weather update server
--  Binds PUB socket to tcp://*:5556
--  Publishes random weather updates
--
--  ZeJiang Tang <kasicass@gmail.com>
--

require("zmq")

-- Prepare our context and publisher
local context = zmq.init(1)
local publisher = context:socket(zmq.PUB)
publisher:bind("tcp://*:5556")
publisher:bind("ipc://weather.ipc")

-- Initialize random number generator
math.randomseed(os.time())
while true do
    -- Get values that will fool the boss
    local zipcode     = math.random(1, 100000)
    local temperature = math.random(1, 215) - 80
    local relhumidity = math.random(1, 50) + 10

    publisher:send(string.format("%05d %d %d", zipcode, temperature, relhumidity))
end

