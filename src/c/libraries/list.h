#ifndef _LIST_H_
#define _LIST_H_

enum fdl_type { fdl_str, fdl_path, fdl_int, fdl_bool };

struct Node {
	enum fdl_type type;
    union {
        int int_item;
        int bool_item;
        char *string_item;
        char *path_item;
    };
    struct Node *next;
};

struct List {
    struct Node *head;
};

struct Node *createIntNode(int data, enum fdl_type type);
struct Node *createStrNode(char *data, enum fdl_type type);

static inline void initList(struct List *list)
{
    list->head = 0;
}

static inline int isEmptyList(struct List *list)
{
    return (list->head == 0);
}

void addFront(struct List *list, struct Node *node);
void traverseList(struct List *list, void (*f)(struct Node *));
void printNode(struct Node *node);
int findNode(struct List *list, struct Node *node1);
void removeNode(struct List *list, struct Node *node1);
struct Node popFront(struct List *list);
void removeAllNodes(struct List *list);
void addAfter(struct List *list, struct Node *prevNode, struct Node *newNode);
void reverseList(struct List *list);
void addBack(struct List *list, struct Node *newNode);
void loadDirectoryToList(char *path, struct List *subPath);



#endif
