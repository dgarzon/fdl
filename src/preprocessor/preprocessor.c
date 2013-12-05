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
    if (argc != 3) {
        fprintf(stderr, "%s\n", "usage: ./preprocessor <fdl file> <fdlp file>");
        exit(1);
    }
    char *fileName = (char *) argv[1];
    char *outputFileName = (char *) argv[2];

    // check input file extension
    if (strcmp("fdl", getFileExtension(fileName)) != 0)
    {
        die("file extension must be fdl");
    }

    // check output file extension
    if (strcmp("fdlp", getFileExtension(outputFileName)) != 0)
    {
        die("output file extension must be fdlp");
    }

    FILE *input;
    if ((input = fopen(fileName, "r")) == NULL) {
        die("fpen() failed");
    }

    // char *outputFileName = "output.fdlp";
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
        else if (strstr(buffer, "string ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "bool ") != NULL) {
            fprintf(output, "%s;\n", buffer);
        }
        else if (strstr(buffer, "for ") != NULL) {
            fprintf(output, "%s {\n", buffer);
        }
        else if (strstr(buffer, "if (") != NULL || strstr(buffer, "if(") != NULL) {
            fprintf(output, "%s {\n", buffer);
        }
        else if (strstr(buffer, "else") != NULL) {
            int i;
            int counter = 0;
            for (i = 0; i < strlen(buffer); ++i)
            {
                if (buffer[i] == ' ') {
                    fprintf(output, "%c", buffer[i]);
                    counter++;
                }
            }
            fprintf(output, "} %s {\n", buffer + counter);
        }
        else if (strstr(buffer, "while (") != NULL || strstr(buffer, "while(") != NULL) {
            fprintf(output, "%s {\n", buffer);
        }
        else if (strstr(buffer, "end") != NULL) {
            int i;
            for (i = 0; i < strlen(buffer); i++){
                if (buffer[i] == 'e') {
                    buffer[i] = '}';
                } else if (buffer[i] == 'n') {
                    buffer[i] = '\n';
                } else if (buffer[i] == 'd') {
                    buffer[i] = '\0';
                } else {

                }
            }
            fprintf(output, "%s", buffer);
        }
        else {
            if (strlen(buffer) > 1)
                fprintf(output, "%s;\n", buffer);
            else
                fprintf(output, "\n");
        }

        if (indentLevel < initialIndentLevel) {
            fprintf(output, "\n}\n");
        }
    }
    fclose(input);
    fclose(output);
    return 0;
}