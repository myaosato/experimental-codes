#include <stdio.h>

int tak(int x, int y, int z)
{
    if (x <= y) {
        return z;
    } else {
        return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
    }   
}

int main(void)
{
    printf("%d\n", tak(24, 12, 0));
    return 0;
}
