
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
struct List temp_list;
int tempint;

int main()
{
struct List * l;
l = &temp_list;
initList(&temp_list);
addBack(&temp_list,createStrNode("a",fdl_str));
addBack(&temp_list,createStrNode("b",fdl_str));
addBack(&temp_list,createIntNode(1,fdl_int));
addBack(&temp_list,createIntNode(2,fdl_int));
addBack(&temp_list,createIntNode(3,fdl_int));
;
if(findNode(l,createStrNode("a",fdl_str)) == 0)
traverseList(l,&printNode);
else
{

}return 0;
}
