require("Init")

local handler = Common.dispatch.newhandler()

local host = arg[1] or "*"
local port = arg[2] or 8080

local server = assert(handler.tcp())
assert(server:setoption("reuseaddr", true))
assert(server:bind(host, port))
assert(server:listen(32))
handler:start(function()
    while true do
        local client = assert(server:accept())
        print("peer connected:", client)
        assert(client:settimeout(0))
        handler:start(function()
            while true do
                local line, err = client:receive()
                if err and err == 'closed' then
                    print("peer closed:", client)
                    Manager.CmdHandler.PeerClose(client)
                    return
                end
                local s = string.split(line)
                Manager.CmdHandler.ProcessCmd(client, unpack(s))
            end
        end)
    end
end)

print(string.format("MMOLite start: %s:%d ...", host, port))
while true do
    handler:step()
end

