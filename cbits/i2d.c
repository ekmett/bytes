
unsigned long long doubleToWord64(double input) {
  union {
    double d;
    unsigned long long l;
  } u;
  u.d = input;
  return u.l;
}

double word64ToDouble(unsigned long long input) {
  union {
    double d;
    unsigned long long l;
  } u;
  u.l = input;
  return u.d;
}

unsigned long long floatToWord32(float input) {
  union {
    float f;
    unsigned long long l;
  } u;
  u.f = input;
  return u.l;
}

float word32ToFloat(unsigned long long input) {
  union {
    float f;
    unsigned long long l;
  } u;
  u.l = input;
  return u.f;
}
