#include "mmio.h"
#include <stdio.h>
#define ADDER_STATUS 0x2000
#define ADDER_X 0x2004
#define ADDER_Y 0x2008
#define ADDER_SUM 0x200C

unsigned int adder_ref(unsigned int x, unsigned int y) {
    x = x + y;
    return x;
}

int main(void)
{
    uint32_t result , ref, x = 4, y = 5;
    // wait for peripheral to be ready
    while ((reg_read8(ADDER_STATUS) & 0x2) == 0) ;

    reg_write32(ADDER_X, x);
    reg_write32(ADDER_Y, y);


    // wait for peripheral to complete
    while ((reg_read8(ADDER_STATUS) & 0x1) == 0) ;

    result = reg_read32(ADDER_SUM);
    ref = adder_ref(x, y);

    if (result != ref) {
      printf("Hardware result %d does not match reference value %d\n", result, ref);
      return 1;
    }
    return 0;
}