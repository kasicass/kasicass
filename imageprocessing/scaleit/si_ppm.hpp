#pragma once

#include "si_image.hpp"
#include <string>

namespace si { namespace io {

ImagePtr readFromPpm(const std::string& filename);
void writeToPpm(ImagePtr img, const std::string& filename);

}}

