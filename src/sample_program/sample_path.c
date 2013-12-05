
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
int a;

int main()
{
char * home;
home = "./sample_program";
printf("%s",home);
printf("%s",getPathName(home));
return 1;
}
