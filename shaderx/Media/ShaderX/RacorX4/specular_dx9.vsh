; diffuse and specular vertex lighting
#define CLIP_MATRIX            0
#define CLIP_MATRIX_1          1
#define CLIP_MATRIX_2          2
#define CLIP_MATRIX_3          3
#define INVERSE_WORLD_MATRIX   4
#define INVERSE_WORLD_MATRIX_1 5
#define INVERSE_WORLD_MATRIX_2 6
#define LIGHT_VECTOR           11
#define EYE_VECTOR             12
#define SPEC_POWER             13
#define SPEC_COLOR             14
#define DIFFUSE_COLOR          15

vs.1.1
dcl_position  v0
dcl_normal    v3
dcl_texcoord  v7

; transpose and transform to clip space
mul r0, v0.x, c[CLIP_MATRIX]
mad r0, v0.y, c[CLIP_MATRIX_1], r0
mad r0, v0.z, c[CLIP_MATRIX_2], r0
add oPos, c[CLIP_MATRIX_3], r0

; output texture coords
mov oT0, v7

; transform normal
dp3 r1.x, v3, c[INVERSE_WORLD_MATRIX]
dp3 r1.y, v3, c[INVERSE_WORLD_MATRIX_1]
dp3 r1.z, v3, c[INVERSE_WORLD_MATRIX_2]

; renormalize it
dp3 r1.w, r1, r1
rsq r1.w, r1.w
mul r1, r1, r1.w

; light vector L
; we need L towards the light, thus negate sign
mov r5, -c[LIGHT_VECTOR]

; N dot L
dp3 r0.x, r1, r5

; compute normalized half vector H = L + V
add r2, c[EYE_VECTOR], r5

; renormalize H
dp3 r2.w, r2, r2
rsq r2.w, r2.w
mul r2, r2, r2.w

; N dot H
dp3 r0.y, r1, r2

; compute specular and clamp values (lit)
; r0.x - N dot L
; r0.y - N dot H
; r0.w - specular power n
mov r0.w, c[SPEC_COLOR].y     ; n must between -128.0 and 128.0
lit r4, r0

; diffuse color * diffuse intensity(r4.y)
mul oD0, c[DIFFUSE_COLOR], r4.y

; specular color * specular intensity(r4.z)
mul oD1, c[SPEC_COLOR], r4.z
