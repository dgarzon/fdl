#include "sys/stat.h"
#include "time.h"
#include "libgen.h"

// test function
void getName(){
	char *dirc, *basec, *bname, *dname;
	char *path = "./list.c";

	dirc = strdup(path);
	basec = strdup(path);
	dname = dirname(dirc);
	bname = basename(basec);
	printf("dirname=%s, basename=%s\n", dname, bname);
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
char* getPathName(char *path){
	if(checkValid(path)){
		char *basec = strdup(path);
		char *bname = basename(basec);
		return bname;
	}else
		return NULL;
}

int main(int argc, char const *argv[])
{
	/* testing the stat sys call for files and directories */
	struct stat info;

	// if (stat(".", &info) != 0)
	// if (stat("./stat_calls.c", &info) != 0)
	if (stat("./list.c", &info) != 0)
		perror("stat() error");
	else {
		puts("stat() returned the following information about the current directory:");
		printf("  inode:   %d\n",   (int) info.st_ino);
		printf(" dev id:   %d\n",   (int) info.st_dev);
		printf("   mode:   %08x\n",       info.st_mode);
		printf("  links:   %d\n",         info.st_nlink);
		printf("    uid:   %d\n",   (int) info.st_uid);
		printf("    gid:   %d\n",   (int) info.st_gid);

		printf("last modified:   %d\n",   (int) info.st_mtime);
		printf("last access:   %d\n",   (int) info.st_atime);
		printf("is directory: %d\n", S_ISDIR(info.st_mode));
		printf("created: %d\n", (int) info.st_birthtime);

		char buf[100];
		struct tm * ptr = localtime(&info.st_mtime);
		strftime(buf, 100, "%a %Y-%m-%d %H:%M:%S %Z", ptr);
    	printf("%s\n", buf);

    	int validPath = checkValid(argv[1]);
    	printf("%s %d\n", argv[1], validPath);
    	// getName();
    	printf("%s\n", getPathName("."));
    	printf("%s\n", getPathName("./list.c"));
	}
	return 0;
}

