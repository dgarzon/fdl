
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
#include "path.h"
 struct List temp_list;
struct Node *node;
struct List subPathList;

int main()
{
struct List * l;
char * b;
char * a;
int check;
char * file2;
char * file1;
char * dir2;
char * dir1;
l = &temp_list;
initList(&temp_list);
;
dir1 = "/Users/rupayanbasu/Documents/Courses/PLT/fdl/fdl/src/sample_program";
dir2 = "/Users/rupayanbasu/Documents/Courses/PLT/fdl/fdl/src";
initList(&subPathList);
loadDirectoryToList(dir2, &subPathList);
node = subPathList.head;
while(node){
file1 = node->string_item;
{
a = getPathName(file1);
addBack(l, createStrNode(strdup(a),fdl_str));
;

}node = node->next;
}
removeNode(l, createStrNode(".",fdl_str));
;
removeNode(l, createStrNode("..",fdl_str));
;
removeNode(l, createStrNode(".DS_Store",fdl_str));
;
check = 0;
initList(&subPathList);
loadDirectoryToList(dir1, &subPathList);
node = subPathList.head;
while(node){
file2 = node->string_item;
{
b = getPathName(file2);
if(findNode(l,createStrNode(b,fdl_str)) == 0)
{
printf("%s\n","duplicate found\n");
printf("%s\n",b);
check = 1;

}else
{

}
}node = node->next;
}
if (check != 1)
{
printf("%s\n","No duplicates found\n");

}return 0;
}
