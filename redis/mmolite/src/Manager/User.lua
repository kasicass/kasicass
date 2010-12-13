-- fd ==> UserObj

module("Manager.User", package.seeall)

local DAOUser = DAO.User
local Mgr = {}

function IsExists(UserID)
    return DAOUser.IsExists(UserID)
end

function GetUserByID(UserID)
    if DAOUser.IsExists(UserID) then
        return DAOUser.GetUserByID(UserID)
    end
end

function CreateUserByID(UserID)
    return DAOUser.GetUserByID(UserID)
end

function RemoveUserByID(UserID)
    DAOUser.RemoveUserByID(UserID)
end

