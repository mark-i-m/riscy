#include <stdio.h>

int main() {
  int i;
  long *x, *y;

  x = (long *)0x200;
  y = (long *)0x400;

  for (i=0; i < 512; i++) {
    y[i] += x[i];
  }

  for (i = 0; i < 512; i++) {
    printf("%ld\n", y[i]);
  }

  return 0;
}
