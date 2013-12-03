#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#define LIST_SIZE 500
#define ARRAY_SIZE 100
int main(){

	enum fdl_type { fdl_int, fdl_str };
	struct fdl_item {
		enum fdl_type type;
		union {
			int int_item;
			int bool_item;
			char *string_item;
			char *path_item;
		};
	};

	//declaring fdl lists
	struct fdl_item fdl_listvar1[LIST_SIZE];
	int li_listvar1 = 0;
	
	struct fdl_item fdl_listvar2[LIST_SIZE];
	int li_listvar2 = 0;

	//initializing fdl list var
	fdl_listvar1[li_listvar1].type = fdl_int;
	fdl_listvar1[li_listvar1].int_item = 1;
	li_listvar1++;

	char *temp_str = (char *)malloc(sizeof(char) * (7+1));
	strcpy(temp_str,"Rupayan");
	fdl_listvar1[li_listvar1].type = fdl_str;
	fdl_listvar1[li_listvar1].string_item = temp_str;
	li_listvar1++;


	//printing list elements
	for(int i=0; i<li_listvar1; i++){
		switch(fdl_listvar1[i].type) {
			case fdl_int: printf("%d ",fdl_listvar1[i].int_item); break;
			case fdl_str: printf("%s \n",fdl_listvar1[i].string_item); break;
		}
	}
	
}