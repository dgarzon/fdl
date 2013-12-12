
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "list.h"
#include "path.h"
struct List temp_list;
int tempint;
int a;

int main()
{
char* home,output;
home = "/Users/rupayanbasu/Documents/Courses/PLT/fdl/fdl/src";

printf("%s",getPathName(home));
return 1;
}
