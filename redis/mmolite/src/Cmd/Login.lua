local UserMgr = Manager.User
local ConnMgr = Manager.Connection

-- LOGIN <username> <password>
function Login(peer, username, password)
    if not username or not password then
        peer:sendline("need username/password!")
        return
    end

    if ConnMgr.GetUserObj(peer) then
        peer:sendline("already login!")
        return
    end

    local UserObj = UserMgr.GetUserByID(username)
    if not UserObj then
        peer:sendline("username not exists!")
        return
    end

    if password ~= UserObj:GetPwd() then
        peer:sendline("password error!")
        return
    end

    ConnMgr.Add(peer, UserObj)
    peer:sendline("login ok!")

    print("login: "..username)
end

