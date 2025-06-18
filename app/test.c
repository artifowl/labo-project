#include <stdio.h>
#include <string.h>

int main() {

    // Define a string
    char str[] = "Hello, World!";
    // Find the length of the string
    size_t length = strlen(str);
    // Print the length of the string
    printf("The length of the string is: %zu\n", length);
    // Print the string itself
    printf("The string is: %s\n", str);
    // Print a message indicating the end of the program
    printf("End of program.\n");
    // Return 0 to indicate successful completion
    printf("Program completed successfully.\n");

    return 0;
}