#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
#include "path.c"
struct List temp_list;
int tempint;
int a;

int main()
{
char * home;
home = "./sample_program";
printf("%s",home);
printf("%s",getPathName(home));
return 1;
}