#include <stdio.h>

extern int read_int(void) {
	int result;

	printf("> ");
	fflush(stdout);
	scanf("%d", &result);

	return result;
}

extern void write_int(int n) {
	printf("%d\n", n);
	fflush(stdout);
}
