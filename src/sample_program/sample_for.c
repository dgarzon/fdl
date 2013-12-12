
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
struct List temp_list;
int tempint;

int main()
{
int count;
char * dir;
char * f;
dir = "/Users/rupayanbasu/Documents/Courses/PLT/fdl/fdl/src";
count = 0;
struct List subPathList; 
initList(&subPathList);
loadDirectoryToList(dir, &subPathList);
struct Node *node = subPathList.head;
while(node){
f = node->string_item;
{
printf("%d",count);
printf("%s",f);
printf("%s","\n");
count = count + 1;

}node = node->next;
}
return 1;
}
