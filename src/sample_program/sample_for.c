
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
struct List temp_list;
int tempint;

int main()
{
char * f;
char * dir;
char * file2;
char * file;
file = "/Users/cjborenstein/Desktop/file.txt";
file2 = "/Users/cjborenstein/Desktop/file2.txt";
dir = "./c";

struct List subPathList; 

initList(&subPathList);

loadDirectoryToList(dir, &subPathList);

struct Node *node = subPathList.head;

while(node){
f = node->string_item;
{
printf("%s\n",f);

}node = node->next;
}
return 1;
}
