function Login(peer, username, password)
    if not username or not password then
        peer:send("need username/password!\n")
        return
    end
    peer:send("login ok!")
end

