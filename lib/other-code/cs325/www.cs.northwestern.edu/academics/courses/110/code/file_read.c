
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    /* dataFile is a string variable with the file name */
    char *dataFile = "sample.dat";
    
    /* fp is a FILE pointer variable for fopen() and fclose() */
    FILE *fp = fopen(dataFile, "r");
    
    /* exitStatus will be set to failure if problems arise */
    int exitStatus;
    
    if (!fp) {
        printf("Couldn't open %s\n", dataFile);
        exitStatus = EXIT_FAILURE;
    }
    else {
        int ioResult;
        int id;
        int age;
        
        /* A typical input loop: read, test, process, read.
         * fscanf() returns either EOF or number of objects read.
         * EOF means end of file. 2 means 2 object read (the correct
         * number. Anything else means something went wrong.
         */
        ioResult = fscanf(fp, " %d %d", &id, &age);
        while (ioResult != EOF && ioResult == 2) {
            printf("ID: %d -- Age: %d\n", id, age);
            ioResult = fscanf(fp, " %d %d", &id, &age);
        }
        
        /* Close the file so others can use it. */
        fclose(fp);
        
        /* If the loop stopped with end of file, everything was read. */
        if (ioResult == EOF) {
            printf("Done\n");
            exitStatus = EXIT_SUCCESS;
        }
        /* Otherwise something went wrong. */
        else {
            printf("Input data error\n");
            exitStatus = EXIT_FAILURE;
        }
    }
    
    getchar();
    return exitStatus;
}

        
