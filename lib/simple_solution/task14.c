#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX_NUMBER 1000000

int main() {
    uint64_t n;
    uint64_t lens_mass[MAX_NUMBER] = {0};
    int32_t count;
    int32_t max = 0;
    uint64_t ans = 0;
    
    for (int i = 1; i < MAX_NUMBER; i++) {
        n = i;
        count = 0;

        while (n > 1) {
            if (n < MAX_NUMBER && lens_mass[n]) {
                count += lens_mass[n];
                break;
            }
            n = (n % 2 == 0) ? n / 2 : 3 * n + 1;
            count++;
        }

        if (i < MAX_NUMBER) lens_mass[i] = count;

        if (max < count) {
            max = count;
            ans = i;
        }
    }

    printf("Problem 14: %lu\n", ans);
    return 0;
}