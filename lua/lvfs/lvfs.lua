-- lvfs, lua virtual file system
-- by kasicass

local lfs = require("lfs")
local pairs = pairs
local print = print
local assert = assert
local string = string
local table  = table
local os = os
local io = io
module("lvfs")


--
-- util func
--

local function isdir(ospath)
	local attr = lfs.attributes(ospath)
	return attr["mode"] == "directory"
end

local function isfile(ospath)
	local attr = lfs.attributes(ospath)
	return attr["mode"] == "file"
end


--
-- ospath/mount
--

local function vfs_ospath(self, vpath)
	assert(string.sub(vpath,1,1) == "/")
	local key = vpath
	local i = string.find(vpath, "/", 2)
	if i then
		key = string.sub(vpath, 1, i-1)
		return self._vpath2ospath[key] .. string.sub(vpath, i)
	else
		return self._vpath2ospath[key]
	end
end

local function vfs_mount(self, vpath, ospath)
	assert(string.sub(vpath,1,1) == "/")
	assert(isdir(ospath))
	self._vpath2ospath[vpath] = ospath
end


--
-- isdir, isfile, listdir
--

local function vfs_isdir(self, vpath)
	return isdir(self:ospath(vpath))
end

local function vfs_isfile(self, vpath)
	return isfile(self:ospath(vpath))
end

local function vfs_listdir(self, vpath)
	local ospath = self:ospath(vpath)
	local _, dirobj = lfs.dir(ospath)
	local files = {}
	local v = dirobj:next()
	while v do
		if v ~= "." and v ~= ".." then
			table.insert(files, v)
		end
		v = dirobj:next()
	end
	return files
end


--
-- dir: mkdir, rmdir
--

local function vfs_mkdir(self, vpath)
	return lfs.mkdir(self:ospath(vpath))
end

local function vfs_rmdir(self, vpath)
	return lfs.rmdir(self:ospath(vpath))
end


--
-- file: open, touch, remove, move, copy
--

local function vfs_open(self, vpath, mode)
	return io.open(self:ospath(vpath), mode)
end

local function vfs_touch(self, vpath)
	local path = self:ospath(vpath)
	local f = io.open(path, "rb")
	if f then
		-- update access/modify time
		f:close()
		lfs.touch(path)
	else
		-- create an empty file
		f = io.open(path, "wb+")    
		f:close()
	end
end

local function vfs_remove(self, vpath)
	return os.remove(self:ospath(vpath))
end

local function vfs_move(self, vpath1, vpath2)
	return os.rename(self:ospath(vpath1), self:ospath(vpath2))
end

local function vfs_copy(self, vpath1, vpath2)
	local path1 = self:ospath(vpath1)
	local path2 = self:ospath(vpath2)
	local f1 = io.open(path1, "rb")
	assert(f1)
	local f2 = io.open(path2, "wb+")
	assert(f2)

	local tmp = f1:read(4196)
	while tmp do
		f2:write(tmp)
		tmp = f1:read(4196)
	end
	f1:close()
	f2:close()
end


--
-- export func
--

function new_vfs()
	return {
		-- data
		_vpath2ospath = {},

		-- func
		mount   = vfs_mount,
		ospath  = vfs_ospath,

		isdir   = vfs_isdir,
		isfile  = vfs_isfile,
		listdir = vfs_listdir,

		mkdir   = vfs_mkdir,
		rmdir   = vfs_rmdir,

		open    = vfs_open,
		touch   = vfs_touch,
		remove  = vfs_remove,
		move    = vfs_move,
		copy    = vfs_copy,
	}
end

