float4x4 WorldViewProj;

struct VS_INPUT
{
	float4 Pos      : POSITION0;
	float2 TexCoord : TEXCOORD0;
};

struct VS_OUTPUT
{
	float4 Pos      : POSITION0;
	float2 TexCoord : TEXCOORD0;
};

VS_OUTPUT VSShaderMain(VS_INPUT Input)
{
	VS_OUTPUT Out = (VS_OUTPUT)0;
	Out.Pos       = mul(Input.Pos, WorldViewProj);
	Out.TexCoord  = Input.TexCoord;
	return Out;
}

