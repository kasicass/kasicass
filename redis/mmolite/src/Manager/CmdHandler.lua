module("Manager.CmdHandler", package.seeall)

local CmdTbl = {
["LOGIN"]   = Cmd.Login, 
["NEWUSER"] = Cmd.NewUser, 
["LOGOUT"]  = Cmd.Logout, 
["RMUSER"]  = Cmd.RemoveUser, 
}

function ProcessCmd(peer, cmd, ...)
    cmd = string.upper(cmd)
    local fn = CmdTbl[cmd]
    if not fn then
        peer:sendline("cmd err!")
        return
    end

    fn(peer, ...)
end

function PeerClose(peer)
    Cmd.LogoutInternal(peer)
end
