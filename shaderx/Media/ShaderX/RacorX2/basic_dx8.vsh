// reg c4-7 = WorldViewProj matrix
// reg c8   = constant color
// reg v0   = input register (position here)

vs.1.1
dp4 oPos.x, v0, c4    // oPos = localPos * WorldViewProj
dp4 oPos.y, v0, c5
dp4 oPos.z, v0, c6
dp4 oPos.w, v0, c7
mov oD0, c8           // material color = c8