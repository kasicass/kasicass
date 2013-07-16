#ifndef K1R_RENDERER_SCENE_H
#define K1R_RENDERER_SCENE_H

#include "K1RBase.h"
#include "K1RSharedPtr.h"

namespace K1R {

// base class, a renderable object
class SceneObject
{
public:
	SceneObject();
	virtual ~SceneObject();


};

// a scene, containing lots of objects to render
class Scene
{
public:
	Scene();
	virtual ~Scene();

	virtual void AddObject(
};

typedef SharedPtr<Scene> ScenePtr;

// global func

};

#endif