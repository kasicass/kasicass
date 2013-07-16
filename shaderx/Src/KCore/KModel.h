#ifndef KCORE_MODEL_H
#define KCORE_MODEL_H

#include <string>

// simple .obj model loader
struct KModel;

struct KModel *KLoadModel(const std::string &path);
void KDrawModel(struct KModel *obj);
void KDestroyModel(struct KModel *obj);

#endif