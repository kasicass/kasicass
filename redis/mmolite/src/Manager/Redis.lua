require("Manager/redis")

local params = {
    host = '127.0.0.1',
    port = 6379,
}

RedisMgr = Redis.connect(params)
