#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"
#include "dirent.h"

void loadDirectoryToList(char *path, struct List *subPath){
    char *buffer;
    DIR *dir;
    struct dirent *ent;
    int len;
    if ((dir = opendir (path)) != NULL) {
        /* print all the files and directories within directory */
        while ((ent = readdir (dir)) != NULL) {
            len = strlen(path) + strlen(ent->d_name) + 2;
            buffer = (char *)malloc(sizeof(char)*len);
            //printf("%s\n",ent->d_name);
            strcpy(buffer, path);
            strcat(buffer, "/");
            strcat(buffer, ent->d_name);
            struct Node * node = createStrNode(buffer, fdl_path);
            addBack(subPath, node);
            //buffer = "\0";
        }
        closedir (dir);
    } else {
        /* could not open directory */
        perror ("");
        exit(0);
    }
}

struct Node *createIntNode(int data, enum fdl_type type) {
    struct Node *newNode = (struct Node *)malloc(sizeof(struct Node));
    if(newNode == NULL){
        printf("Could not create new node!\n");
        exit(1);
    }

    newNode->type = type;
    newNode->next = NULL;
    switch(newNode->type){
        case fdl_int: newNode->int_item = data; break;
        case fdl_bool: newNode->bool_item = data; break;
        default: break;
    }
    return newNode;
}

struct Node *createStrNode(char *data, enum fdl_type type) {
    struct Node *newNode = (struct Node *)malloc(sizeof(struct Node));
    if(newNode == NULL){
        printf("Could not create new node!\n");
        exit(1);
    }

    newNode->type = type;
    newNode->next = NULL;
    switch(newNode->type){
        case fdl_str: newNode->string_item = data; break;
        case fdl_path: newNode->path_item = data; break;
        default: break;
    }
    return newNode;
}

void addFront(struct List *list, struct Node *node)
{
    node->next = list->head;
    list->head = node;
}

void traverseList(struct List *list, void (*f)(struct Node *))
{
    struct Node *node = list->head;
    while (node) {
    f(node);
    node = node->next;
    }
}

void printNode(struct Node *node)
{
    switch(node->type){
        case fdl_int: printf("%d\n",node->int_item); break;
        case fdl_bool: if(node->bool_item == 1) printf("True\n");
                        else printf("False\n"); break;
        case fdl_str: printf("%s\n",node->string_item); break;
        case fdl_path: printf("%s\n",node->path_item); break;
    }
}

int findNode(struct List *list, struct Node *node1) {
    struct Node *node2 = list->head;
    while (node2) {
        if(node1->type == node2->type){
            switch(node1->type){
                case fdl_int: if (node1->int_item == node2->int_item) return 0; else break;
                case fdl_str: if (strcmp(node1->string_item, node2->string_item) == 0) return 0; else break;
                case fdl_bool: if (node1->bool_item == node2->bool_item) return 0; else break;
                case fdl_path: if (strcmp(node1->path_item, node2->path_item) == 0) return 0; else break;
                default: return 1;
            }
        }
        node2 = node2->next;
    }
    return 1;
}

void removeNode(struct List *list, struct Node *node1) {
    struct Node *node2 = list->head;
    int del = 0;
    struct Node *prev = list->head;
    while (node2) {
        if(node1->type == node2->type){
            switch(node1->type){
                case fdl_int: if (node1->int_item == node2->int_item) { del = 1; break; } else break;
                case fdl_str: if (strcmp(node1->string_item, node2->string_item) == 0) { del = 1; break; } else break;
                case fdl_bool: if (node1->bool_item == node2->bool_item) { del = 1; break; } else break;
                case fdl_path: if (strcmp(node1->path_item, node2->path_item) == 0) { del = 1; break; } else break;
                default: del = 0;
            }
        }
        if(del == 0){
            prev = node2;
            node2 = node2->next;
        }
        else break;
    }
    if(del == 1){
        if(node2 == list->head)
            list->head = node2->next;
        else
            prev->next = node2->next;
        free(node2);
    }
    
}

struct Node popFront(struct List *list) {
    struct Node *oldHead = list->head;
    struct Node node = *oldHead;
    list->head = oldHead->next;
    free(oldHead);
    return node;
}

void removeAllNodes(struct List *list)
{
    while (!isEmptyList(list))
    popFront(list);
}

void addAfter(struct List *list,
    struct Node *prevNode, struct Node *newNode)
{
    if (prevNode == NULL)
       addFront(list, newNode);

    newNode->next = prevNode->next;
    prevNode->next = newNode;
}

void reverseList(struct List *list)
{
    struct Node *prv = NULL;
    struct Node *cur = list->head;
    struct Node *nxt;

    while (cur) {
    nxt = cur->next;
    cur->next = prv;
    prv = cur;
    cur = nxt;
    }

    list->head = prv;
}

void addBack(struct List *list, struct Node *newNode)
{
    newNode->next = NULL;

    if (list->head == NULL) {
       list->head = newNode;
       return;
    }

    struct Node *end = list->head;
    while (end->next != NULL)
       end = end->next;

    end->next = newNode;
}

