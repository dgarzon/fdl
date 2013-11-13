#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

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

        size_t len = strlen(buffer) - 1;
        if (buffer[len] == '\n') {
            buffer[len] = '\0';
        }

        if (strstr(buffer, "def ") != NULL) {
            fprintf(output, "%s {\n", buffer);
            indentLevel++;
        }
        else if (strstr(buffer, "int ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "path ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "dict ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "list ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "for ") != NULL) {
            fprintf(output, "%s {\n", buffer);
        }
        else if (strstr(buffer, "if (") != NULL || strstr(buffer, "if(") != NULL) {
            fprintf(output, "%s {\n", buffer);
        }
        else if (strstr(buffer, "while (") != NULL || strstr(buffer, "while(") != NULL) {
            fprintf(output, "%s {\n", buffer);
        } else {
            if (strlen(buffer) > 1)
                fprintf(output, "%s;\n", buffer);
        }

        if (indentLevel < initialIndentLevel) {
            fprintf(output, "\n}\n");
        }

        printf("%s\n", buffer);
    }
    fclose(input);
    fclose(output);
    return 0;
}