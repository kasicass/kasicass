; per-vertex diffuse lighting

#define CLIP_MATRIX            0
#define CLIP_MATRIX_1          1
#define CLIP_MATRIX_2          2
#define CLIP_MATRIX_3          3
#define INVERSE_WORLD_MATRIX   4
#define INVERSE_WORLD_MATRIX_1 5
#define INVERSE_WORLD_MATRIX_2 6
#define LIGHT_POSITION         7
#define DIFFUSE_COLOR          14
#define LIGHT_COLOR            15

vs.1.1
dcl_position v0
dcl_normal   v3

; transpose and transform to clip space
mul r0, v0.x, c[CLIP_MATRIX]
mad r0, v0.y, c[CLIP_MATRIX_1], r0
mad r0, v0.z, c[CLIP_MATRIX_2], r0
add oPos, c[CLIP_MATRIX_3], r0

; transform normal
dp3 r1.x, v3, c[INVERSE_WORLD_MATRIX]
dp3 r1.y, v3, c[INVERSE_WORLD_MATRIX_1]
dp3 r1.z, v3, c[INVERSE_WORLD_MATRIX_2]

; renormal it
dp3 r1.w, r1, r1      ; (src1.x * src2.x) + (src1.y * src2.y) + (src1.z * src2.z)
rsq r1.w, r1.w        ; if (v != 0 && v != 1.0) v = (float)(1.0f / sqrt(v))
mul r1, r1, r1.w      ; r1 * r1.w

; N dot L
; we need L vector towards the light, thus negate sign
dp3 r0, r1, -c[LIGHT_POSITION]
mul r0, r0, c[LIGHT_COLOR]           ; modulate against light color
mul oD0, r0, c[DIFFUSE_COLOR]        ; modulate against material
