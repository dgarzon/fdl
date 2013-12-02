#ifndef _LIST_H_
#define _LIST_H_

struct Node {
    void *data;
    struct Node *next;
};


struct List {
    struct Node *head;
};


static inline void initList(struct List *list)
{
    list->head = 0;
}


struct Node *addFront(struct List *list, void *data);


struct Node *addBack(struct List *list, void *data);


void traverseList(struct List *list, void (*f)(void *));

struct Node *findNode(struct List *list, const void *dataSought,
	int (*compar)(const void *, const void *));

void flipSignDouble(void *data);

int compareDouble(const void *data1, const void *data2);

static inline int isEmptyList(struct List *list)
{
    return (list->head == 0);
}

void *popFront(struct List *list);

void removeAllNodes(struct List *list);

struct Node *addAfter(struct List *list, 
	struct Node *prevNode, void *data);

void reverseList(struct List *list);

#endif
