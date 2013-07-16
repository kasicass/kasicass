#include "K1RBase.h"

#if K1R_WIN32
#include "K1RDX9Shader.h"
#endif

namespace K1R {

VertexShaderPtr MakeVertexShader(const std::string &resourceID)
{
#if K1R_WIN32
	return MakeDX9VertexShader(resourceID);
#endif
}

PixelShaderPtr MakePixelShader(const std::string &resourceID)
{
#if K1R_WIN32
	return MakeDX9PixelShader(resourceID);
#endif
}

}