
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
char * f;
char * html;
char * imgs;
char * css;
char * js;
char * src;
src = "./demo/site";
js = "./demo/site/js";
css = "./demo/site/css";
imgs = "./demo/site/imgs";
html = "./demo/site/html";
initList(&subPathList);
loadDirectoryToList(src, &subPathList);
node = subPathList.head;
while(node){
f = node->string_item;
{
if (!strcmp(getExtension(f), ".js"))
{
moveFile(f, js);

}if (!strcmp(getExtension(f), ".css"))
{
moveFile(f, css);

}if (!strcmp(getExtension(f), ".jpeg"))
{
moveFile(f, imgs);

}if (!strcmp(getExtension(f), ".html") && strcmp(getPathName(f), "index.html"))
{
moveFile(f, html);

}
}node = node->next;
}
return 0;
}
