; real-time glow

; c0-c7,  
; c8-c11, clip matrix

vs.2.0
dcl_position v0
dcl_texcoord v3

; transform pos
mul r0, v0.x, c8
mad r0, v0.y, c9, r0
mad r0, v0.z, c10, r0
add oPos, c11, r0

; texcoord
mov oT0, v3
