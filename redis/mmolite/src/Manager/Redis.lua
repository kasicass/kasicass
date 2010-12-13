module("Manager.Redis", package.seeall)

local redis = require("Common.redis")

local params = {
    host = '127.0.0.1',
    port = 6379,
}

Instance = redis.connect(params)
