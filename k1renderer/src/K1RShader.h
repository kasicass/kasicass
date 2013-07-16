#ifndef K1R_RENDERER_SHADER_H
#define K1R_RENDERER_SHADER_H

#include <memory>
#include <string>
#include "K1RMatrix.h"

namespace K1R {

class VertexShader
{
public:
	VertexShader() {}
	virtual ~VertexShader() {}

	virtual void setMatrix(const std::string &name, const Matrix &mat) = 0;

	virtual void useMe() = 0;
};
typedef std::shared_ptr<VertexShader> VertexShaderPtr;

class PixelShader
{
public:
	PixelShader() {}
	virtual ~PixelShader() {}

	virtual void setMatrix(const std::string &name, const Matrix &mat) = 0;

	virtual void useMe() = 0;
};
typedef std::shared_ptr<PixelShader> PixelShaderPtr;


VertexShaderPtr MakeVertexShader(const std::string &resourceID);
PixelShaderPtr MakePixelShader(const std::string &resourceID);

}

#endif