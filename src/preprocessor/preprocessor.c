#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_BUFFER 4096

static void die(const char *message)
{
    perror(message);
    exit(1);
}

const char *getFileExtension(const char *fileName) {
    const char *dot = strrchr(fileName, '.');
    if(!dot || dot == fileName) return "";
    return dot + 1;
}

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "%s\n", "usage: ./preprocessor <fdl file>");
        exit(1);
    }
    char *fileName = (char *) argv[1];
    printf("File Name: %s\n", fileName);
    printf("%s\n", getFileExtension(fileName));

    if (strcmp("fdl", getFileExtension(fileName)) != 0)
    {
        die("file extension must be fdl");
    }

    FILE *input;
    if ((input = fopen(fileName, "r")) == NULL) {
        die("fpen() failed");
    }

    char *outputFileName = "output.fdlp";
    FILE *output;
    if ((output = fopen(outputFileName, "w")) == NULL) {
        die("fpen() failed");
    }

    char buffer[MAX_BUFFER];
    int indentLevel = 0;

    while (fgets(buffer, sizeof(buffer), input) != NULL) {
        int initialIndentLevel = indentLevel;

        if (strncmp("def", buffer, 3) == 0) {
            strcat(buffer, "{\n");
            indentLevel++;
        } else {
            strcat(buffer, ";\n");
            indentLevel--;
        }

        if (indentLevel < initialIndentLevel) {
            strcat(buffer, "\n}");
        }
        // fwrite (buffer, 1, sizeof(buffer), output);
        printf("%s", buffer);
    }
    fclose(input);
    fclose(output);
    return 0;
}