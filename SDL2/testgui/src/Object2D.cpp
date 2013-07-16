#include "Object2D.hpp"

struct MessageStatus
{
	bool hover;
};

void Object2D::sendMouseMessagePrivate(int x, int y, MessageStatus *ms)
{
}

void Object2D::sendMouseMessage(int x, int y)
{
	MessageStatus init = { false };
	this->sendMouseMessagePrivate(x, y, &init);
}

