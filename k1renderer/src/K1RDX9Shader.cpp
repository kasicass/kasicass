#include "K1RDX9Shader.h"
#include "K1RAsset.h"
#include <assert.h>

namespace K1R {

//
// DX9VertexShader
//
DX9VertexShader::DX9VertexShader() :
	shader_(nullptr),
	table_(nullptr)
{}

DX9VertexShader::~DX9VertexShader()
{
	DX9::SafeRelease(shader_);
	DX9::SafeRelease(table_);
}

void DX9VertexShader::setMatrix(const std::string &name, const Matrix &mat)
{
	DX9::Device pDevice = DX9::GetDevice();
	D3DXHANDLE hMatrix = table_->GetConstantByName(NULL, name.c_str());
	table_->SetMatrix(pDevice, hMatrix, &mat);
}

void DX9VertexShader::useMe()
{
	DX9::Device pDevice = DX9::GetDevice();
	pDevice->SetVertexShader(shader_);
}

bool DX9VertexShader::createFromString(const char *data, unsigned int len)
{
	HRESULT hr;
	LPD3DXBUFFER pShaderBuffer, pErrBuffer;
	hr = D3DXCompileShader(data, len, NULL, NULL,
		"vsmain", "vs_2_0", D3DXSHADER_DEBUG, &pShaderBuffer, &pErrBuffer, &table_);
	if (FAILED(hr))
	{
		OutputDebugString("D3DXCompileShader Err (vs): ");
		OutputDebugString((char*)pErrBuffer->GetBufferPointer());
		OutputDebugString("\n");
		return false;
	}

	DX9::Device pDevice = DX9::GetDevice();
	hr = pDevice->CreateVertexShader((DWORD*)pShaderBuffer->GetBufferPointer(), &shader_);
	if (FAILED(hr))
	{
		OutputDebugString("Device::CreateVertexShader failed\n");
		DX9::SafeRelease(pShaderBuffer);
		return false;
	}

	DX9::SafeRelease(pShaderBuffer);
	return true;
}


//
// DX9PixelShader
//
DX9PixelShader::DX9PixelShader() :
	shader_(nullptr),
	table_(nullptr)
{
}

DX9PixelShader::~DX9PixelShader()
{
	DX9::SafeRelease(shader_);
	DX9::SafeRelease(table_);
}

void DX9PixelShader::setMatrix(const std::string &name, const Matrix &mat)
{
	DX9::Device pDevice = DX9::GetDevice();
	D3DXHANDLE hMatrix = table_->GetConstantByName(NULL, name.c_str());
	table_->SetMatrix(pDevice, hMatrix, &mat);
}

void DX9PixelShader::useMe()
{
	DX9::Device pDevice = DX9::GetDevice();
	pDevice->SetPixelShader(shader_);
}

bool DX9PixelShader::createFromString(const char *data, unsigned int len)
{
	HRESULT hr;
	LPD3DXBUFFER pShaderBuffer, pErrBuffer;
	hr = D3DXCompileShader(data, len, NULL, NULL,
		"psmain", "vs_2_0", D3DXSHADER_DEBUG, &pShaderBuffer, &pErrBuffer, &table_);
	if (FAILED(hr))
	{
		OutputDebugString("D3DXCompileShader Err (ps): ");
		OutputDebugString((char*)pErrBuffer->GetBufferPointer());
		OutputDebugString("\n");
		return false;
	}

	DX9::Device pDevice = DX9::GetDevice();
	hr = pDevice->CreatePixelShader((DWORD*)pShaderBuffer->GetBufferPointer(), &shader_);
	if (FAILED(hr))
	{
		OutputDebugString("Device::CreatePixelShader failed\n");
		DX9::SafeRelease(pShaderBuffer);
		return false;
	}

	DX9::SafeRelease(pShaderBuffer);
	return true;
}


//
// Factory Function
//
VertexShaderPtr MakeDX9VertexShader(const std::string &resourceID)
{
	FilePtr pFile = Asset::readFile(resourceID);
	DX9VertexShader *pShader = new DX9VertexShader;
	bool ok = pShader->createFromString((char*)pFile->data(), pFile->length());
	assert(ok);
	return VertexShaderPtr(pShader);
}

PixelShaderPtr MakeDX9PixelShader(const std::string &resourceID)
{
	FilePtr pFile = Asset::readFile(resourceID);
	DX9PixelShader *pShader = new DX9PixelShader;
	bool ok = pShader->createFromString((char*)pFile->data(), pFile->length());
	assert(ok);
	return PixelShaderPtr(pShader);
}

}
