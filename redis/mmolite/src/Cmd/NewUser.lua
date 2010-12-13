local UserMgr = Manager.User

-- NEWUSER <username> <password>
function NewUser(peer, username, password)
    if not username or not password then
        peer:sendline("need username/password!")
        return
    end

    if UserMgr.IsExists(username) then
        peer:sendline("username already exists!")
        return
    end

    local UserObj = UserMgr.CreateUserByID(username)
    UserObj:SetName(username)
    UserObj:SetPwd(password)

    peer:sendline("create ok!")
end

