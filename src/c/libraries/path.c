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

// get the last directory or filename
char* getPathName(char* path){
	if(checkValid(path)){
        char *basec = strdup(path);
        char *bname = basename(basec);
        return bname;
    }else
        return NULL;

}

