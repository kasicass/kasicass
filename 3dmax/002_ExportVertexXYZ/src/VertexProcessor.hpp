#pragma once

#include "DllMain.hpp"

class VertexProcessor : public ITreeEnumProc
{
public:
	VertexProcessor(TimeValue t, FILE *out) : t_(t), out_(out) {}

    virtual int callback(INode *node);

private:
    TimeValue t_;
    FILE* out_;
};
