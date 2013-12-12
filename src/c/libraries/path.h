#ifndef _PATH_H_
#define _PATH_H_

char* getName(char *path, char *output);
int checkValid(char *path);
int getCreatedAt(char *path);
int getPathType(char *path);
char* getPathName(char *path);
int copyFile(char* src, char *dest);
int moveFile(char* src, char *dest);

#endif
