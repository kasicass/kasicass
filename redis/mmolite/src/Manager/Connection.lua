module("Manager.Connection", package.seeall)

local Peer2UserObj = {}

function GetUserObj(peer)
    return Peer2UserObj[peer]
end

function GetUserObjByName(cName)
    for _, UserObj in pairs(Peer2UserObj) do
        if UserObj:GetName() == cName then
            return UserObj
        end
    end
end

function Add(peer, UserObj)
    Peer2UserObj[peer] = UserObj
end

function Remove(peer)
    Peer2UserObj[peer] = nil
end

