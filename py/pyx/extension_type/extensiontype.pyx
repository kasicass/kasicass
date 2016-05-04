cdef class Particle(object):
  cdef public double mass
  cdef readonly double position
  cdef double velocity

  def __init__(self, m, p, v):
    self.mass = m
    self.position = p
    self.velocity = v

  def get_momentum(self):
    return self.mass * self.velocity

