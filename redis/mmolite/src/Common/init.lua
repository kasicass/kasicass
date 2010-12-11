-- some util func

string.split = function (str)
    local sfind = string.find
    local ssub  = string.sub

    local ret  = {}

    local s = 1
    while true do
        local i, j = sfind(str, " ", s)
        if i then
            table.insert(ret, ssub(str, s, i-1))
            s = j + 1
        else
            table.insert(ret, ssub(str, s))
            return ret
        end
    end
end
