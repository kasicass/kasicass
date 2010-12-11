local redis = require("Common/redis")

local params = {
    host = '127.0.0.1',
    port = 6379,
}

RedisMgr = redis.connect(params)
