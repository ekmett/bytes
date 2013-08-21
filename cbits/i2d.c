#include <stdint.h>

uint64_t doubleToWord64(double input) {
  union {
    double d;
    uint64_t l;
  } u;
  u.d = input;
  return u.l;
}

double word64ToDouble(uint64_t input) {
  union {
    double d;
    uint64_t l;
  } u;
  u.l = input;
  return u.d;
}

uint32_t floatToWord32(float input) {
  union {
    float f;
    uint32_t l;
  } u;
  u.f = input;
  return u.l;
}

float word32ToFloat(uint32_t input) {
  union {
    float f;
    uint32_t l;
  } u;
  u.l = input;
  return u.f;
}
