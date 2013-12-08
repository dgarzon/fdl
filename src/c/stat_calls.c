#include <stdio.h>
#include "list.h"
#include "path.h"
#include "sys/stat.h"
#include "time.h"
#include "libgen.h"
#include "dirent.h"

void testDirent(){
	// DIR *dir;
	// struct dirent *ent;
	// if ((dir = opendir ("libraries")) != NULL) {
	// 	/* print all the files and directories within directory */
	// 	while ((ent = readdir (dir)) != NULL) {
	// 		printf ("%s\n", ent->d_name);
	// 	}
	// 	closedir (dir);
	// } else {
	// 	/* could not open directory */
	// 	perror ("");
	// 	exit(0);
	// }

}

int main(int argc, char const *argv[])
{
	// printf("%s\n", getPathName("."));
	// printf("%s\n", getPathName("libraries/list.c"));
	/* testing the stat sys call for files and directories */
	struct stat info;

	if (stat("./Makefile", &info) != 0)
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
    	// printf("%s\n", getPathName("."));
    	// printf("%s\n", getPathName("./list.c"));
    	// testDirent();
    	struct List subPathList;
    	initList(&subPathList);
    	loadDirectoryToList("libraries", &subPathList);
    	printf("here \n");
    	traverseList(&subPathList, &printNode);
	}
	return 0;
}

