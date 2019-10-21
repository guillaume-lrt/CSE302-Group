#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void bx1_panic()
{
  fprintf(stderr, "RUNTIME PANIC!\n");
  exit(-1);
}

void bx1_print_int(int64_t x)
{
  printf("%ld\n", x);
}

void bx1_print_bool(int64_t x)
{
  printf("%s\n", x == 0 ? "false" : "true");
}
