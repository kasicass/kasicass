struct VS_OUTPUT
{
	float4 Pos : POSITION0;
};

float4 PSShaderMain(VS_OUTPUT input) : COLOR0
{
	return float4(1.0, 0.0, 0.0, 1.0);
}

