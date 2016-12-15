#include <stdio.h>

int main() {
  int i1 = 0;
	int i2 = 1;
	int i3;
	int j;
	int result;

	for (j=0; j<10000; j++) {
		i3 = i1 + i2;
		result = i3;
		i1 = i2;
		i2 = i3;
	}

	
  return result;
}
