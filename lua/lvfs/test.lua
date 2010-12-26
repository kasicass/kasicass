require("lvfs")
root = lvfs.new_vfs()
root:mount('/hello', '/tmp/lvfs')
local f = root:open('/hello/cool', "rb")
local v = f:read()
while v do
	print(v)
	v = f:read()
end
f:close()
