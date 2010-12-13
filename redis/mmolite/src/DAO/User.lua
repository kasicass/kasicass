module("User", package.seeall)

local R = Manager.Redis.Instance

function IsExists(cUserID)
    local cKey = string.format("p/%s/prop", cUserID)
    return R:exists(cKey)
end

function GetUserByID(cUserID)
    local PropGetter = function (cPropName)
        return function (self)
            local cKey = string.format("p/%s/prop", cUserID)
            return R:hget(cKey, cPropName)
        end
    end

    local PropSetter = function (cPropName)
        return function (self, cValue)
            local cKey = string.format("p/%s/prop", cUserID)
            return R:hset(cKey, cPropName, cValue)
        end
    end

    return {
        GetName = PropGetter("name"),
        GetHp   = PropGetter("hp"),
        GetMp   = PropGetter("mp"),
        GetLv   = PropGetter("lv"),
        GetPwd  = PropGetter("password"),

        SetName = PropSetter("name"),
        SetHp   = PropSetter("hp"),
        SetMp   = PropSetter("mp"),
        SetLv   = PropSetter("lv"),
        SetPwd  = PropSetter("password"),
    }
end

function RemoveUserByID(cUserID)
    local cKey = string.format("p/%s/prop", cUserID)
    return R:del(cKey)
end

