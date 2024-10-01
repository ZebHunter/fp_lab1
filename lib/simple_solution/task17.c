#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int32_t count_let (int32_t n){
    int32_t mas_to_twenty[] = {0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8};
    int32_t mas_to_handred[] = {0, 0, 6, 6, 5, 5, 5, 7, 6, 6};
    int32_t hundred = 7;
    int32_t thousand = 8;
    int32_t land = 3;
    int32_t count = 0;

    count += (n / 1000) ? mas_to_twenty[n / 1000] + thousand : 0;
    n %= 1000;
    count += (n / 100) ? mas_to_twenty[n / 100] + hundred : 0;
    n %= 100;

    if (n && count) count += land;

    if (n < 20) count += mas_to_twenty[n];
    else {
        count += mas_to_handred[n / 10];
        n %= 10;
        count += mas_to_twenty[n];
    }
    
    return count;
}

int main(){
    int32_t count = 0;
    for (int i = 1; i < 1001; i++){
        count += count_let(i);
    }

    printf("Problem 17: %d\n", count);
}