local ConnMgr = Manager.Connection

-- LOGOUT
function LogoutInternal(peer)
    local UserObj = ConnMgr.GetUserObj(peer)
    if UserObj then
        print("logout: "..UserObj:GetName())
        ConnMgr.Remove(peer)
    end
end

function Logout(peer)
    LogoutInternal(peer)

    peer:sendline("bye!")
    peer:close()
end

