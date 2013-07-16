#include "K1RDX9Defines.h"

namespace K1R { namespace DX9 {

namespace {
Device g_d3dDevice = NULL;
}

Device GetDevice() { return g_d3dDevice; }
void SetDevice(Device dev) { g_d3dDevice = dev; }

} }