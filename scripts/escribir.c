#include <stdio.h>
int main() {
    FILE *f = fopen("dato.txt", "w");
    if (f) {
        fprintf(f, "21\n");
        fclose(f);
    }
    return 0;
}
