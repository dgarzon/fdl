#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sys/stat.h"
#include "time.h"
#include<libgen.h>

// test function
char* getName(char *path, char *output){
	char *dirc, *basec, *bname, *dname;

	dirc = strdup(path);
	basec = strdup(path);
	dname = dirname(dirc);
	bname = basename(basec);
	//printf("dirname=%s, basename=%s\n", dname, bname);
	strcpy(output, dname);
	return output;
}

int checkValid(char *path){
	/* testing the stat sys call for files and directories */
	struct stat info;
	if (stat(path, &info) != 0)
		return 0;
	else
		// can be valid directory or file
		return S_ISDIR(info.st_mode) ? 1 : S_ISREG(info.st_mode);
}

// returns -1 in case of invalid path
int getCreatedAt(char *path){
	if(checkValid(path)){
		struct stat info;
		stat(path, &info);

		return (int) info.st_birthtime;
	}else
		return -1;
}

// Directory 1, File 0, invalid path -1
int getPathType(char *path){
	if(checkValid(path)){
		struct stat info;
		stat(path, &info);

		return S_ISDIR(info.st_mode);
	}else
		return -1;
}

int isDir(char *path){
	if(checkValid(path)){
		struct stat info;
		stat(path, &info);

		return S_ISDIR(info.st_mode);
	}else
		return -1;
}

// get the last directory or filename
char* getPathName(char* path){
	if(checkValid(path)){
        char *basec = strdup(path);
        char *bname = basename(basec);
        return bname;
    }else
        return NULL;

}

int copyFile(char* src, char *dest){
	char copycommand[1000];
	if (checkValid(dest) == 0) {
		char temp[1000] = "mkdir -p ";
		strcat(temp, dest);
		system(temp);
	}
	sprintf(copycommand, "/bin/cp %s %s", src, dest);
	return system(copycommand);
}

int moveFile(char* src, char *dest){
	char movecommand[1000];

	if (checkValid(dest) == 0) {
		char temp[1000] = "mkdir -p ";
		strcat(temp, dest);
		system(temp);
	}
	sprintf(movecommand, "/bin/mv %s %s", src, dest);
	return system(movecommand);
}

char* getExtension(char *path){
	char *ptr = rindex(path, '.');
	return strdup(ptr);
}

char* stringConcat(char *str1, char *str2){
	char *strdup1 = strdup(str1);
	char *strdup2 = strdup(str2);
	strcat(strdup1, strdup2);
	return strdup1;
}