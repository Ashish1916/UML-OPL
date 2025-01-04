#include <stdio.h>
#include <stdlib.h>

// Function 1: Memory safety bug in dangling pointer
void test1() {
    int *ptr = (int *)malloc(sizeof(int));
    *ptr = 10;
    free(ptr); // Memory is freed here
    printf("Value after free: %d\n", *ptr); // Dereferencing dangling pointer
}

// Function 2: Memory safety bug in invalid write
void test2() {
    int arr[5] = {1, 2, 3, 4, 5};
    arr[10] = 42; // Writing out of bounds
    printf("Value at arr[10]: %d\n", arr[10]);
}

// Function 3: Use after free
void test3() {
    int *ptr = (int *)malloc(sizeof(int));
    *ptr = 20;
    free(ptr);
    printf("Freed value: %d\n", *ptr); 
}

// Annotated comments:
// test1(): Dereferencing a dangling pointer after the memory is freed. 
// test2(): Writing to an array out of bounds. 
// test3(): Using a pointer after it is freed.