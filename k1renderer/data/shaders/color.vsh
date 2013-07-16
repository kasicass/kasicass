float4x4 matWorldViewProj;

struct VS_INPUT
{
	float4 Pos     : POSITION0;
	float4 Diffuse : COLOR0;
};

struct VS_OUTPUT
{
	float4 Pos     : POSITION0;
	float4 Diffuse : COLOR0;
};

VS_OUTPUT vsmain(VS_INPUT Input)
{
	VS_OUTPUT Out = (VS_OUTPUT)0;
	Out.Pos     = mul(Input.Pos, matWorldViewProj);
	Out.Diffuse = Input.Diffuse;
	return Out;
}

