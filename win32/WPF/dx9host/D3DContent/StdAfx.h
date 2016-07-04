#pragma once

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#include <d3d9.h>
#include <d3dx9.h>

#include <assert.h>

#include "Renderer.h"
#include "RendererManager.h"
#include "TriangleRenderer.h"

#define IFC(x) { hr = (x); if (FAILED(hr)) goto Cleanup; }
#define IFCOOM(x) { if ((x) == NULL) { hr = E_OUTOFMEMORY; IFC(hr); } }
#define SAFE_RELEASE(x) { if (x) { x->Release(); x = NULL; } }