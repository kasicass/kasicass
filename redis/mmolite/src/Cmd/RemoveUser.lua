local UserMgr = Manager.User
local ConnMgr = Manager.Connection

-- RMUSER <username> <password>
function RemoveUser(peer, username, password)
    if not username or not password then
        peer:sendline("need username/password!")
        return
    end

    if ConnMgr.GetUserObjByName(username) then
        peer:sendline("can't delete logined user")
        return
    end

    if not UserMgr.IsExists(username) then
        peer:sendline("username not exists!")
        return
    end

    local UserObj = UserMgr.GetUserByID(username)
    if password ~= UserObj:GetPwd() then
        peer:sendline("password error, can't delete!")
        return
    end

    UserMgr.RemoveUserByID(username)
    peer:sendline("delete ok!")
end

