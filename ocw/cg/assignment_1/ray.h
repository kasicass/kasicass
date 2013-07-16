<html>#ifndef _RAY_H
#define _RAY_H

#include <iostream>
using namespace std;

#include "vectors.h"

// Ray class mostly copied from Peter Shirley and Keith Morley

// ====================================================================
// ====================================================================

class Ray {

public:

  // CONSTRUCTOR & DESTRUCTOR
  Ray () {}
  Ray (const Vec3f &dir, const Vec3f &orig) {
    direction = dir;
    origin = orig; }
  Ray (const Ray& r) {*this=r;}

  // ACCESSORS
  const Vec3f& getOrigin() const { return origin; }
  const Vec3f& getDirection() const { return direction; }
  Vec3f pointAtParameter(float t) const {
    return origin+direction*t; }
  
private:

  // REPRESENTATION
  Vec3f direction;
  Vec3f origin;

};

inline ostream &operator<<(ostream &os, const Ray &r) {
  os << "Ray <" <<r.getOrigin()<<", "<<r.getDirection()<<">";
  return os;
}

// ====================================================================
// ====================================================================

#endif
