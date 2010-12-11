module("Manager.CmdHandler", package.seeall)

local CmdTbl = {
["LOGIN"] = Cmd.Login, 
}

function ProcessCmd(peer, cmd, ...)
    local fn = CmdTbl[cmd]
    if not fn then
        print("err fn: "..cmd)
        return
    end

    fn(peer, ...)
end

function PeerClose(peer)
    print("peer closed:", peer)
end
