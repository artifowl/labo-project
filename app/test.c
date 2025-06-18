#include <stdio.h>
#include <string.h>

int main() {
    int value;
    printf("Value : %d\n", value);

    char small[10];
    strcpy(small, "this text is too long for the buffer");

    char input[20];
    printf("Enter some text: ");
    gets(input);  

    return 0;
}