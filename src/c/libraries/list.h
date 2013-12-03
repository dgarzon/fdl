#ifndef _LIST_H_
#define _LIST_H_

enum fdl_type { fdl_str, fdl_int };

struct Node {
	enum fdl_type type;
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



static inline int isEmptyList(struct List *list)
{
    return (list->head == 0);
}


struct Node *addFront(struct List *list, void *data)
{
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;

    node->data = data;
    node->next = list->head;
    list->head = node;
    return node;
}

void traverseList(struct List *list, void (*f)(struct Node *))
{
    struct Node *node = list->head;
    while (node) {
	f(node);
	node = node->next;
    }
}



static void printInteger(void *p)
{
    printf("%d \n", *(int *)p);
}

static void printString(void *p)
{
    printf("%s \n", (char *)p);
}

static void checkNodeType(struct Node *node)
{
	switch(node->type){
		case fdl_int: printInteger(node->data); break;
		case fdl_str: printString(node->data); break;
	}
}

int compareInteger(const void *data1, const void *data2)
{
    if (*(int *)data1 == *(int *)data2)
	return 0;
    else
	return 1;
}

int compareString(const void *data1, const void *data2) {
    if (strcmp((const char *)data1, (const char *)data2) == 0) {
        return 0;
    }
    else
        return 1;
}

struct Node *findNode(struct List *list, const void *dataSought,
	int (*compar)(const void *, const void *))
{
    struct Node *node = list->head;
    while (node) {
	if (compar(dataSought, node->data) == 0)
	    return node;
	node = node->next;
    }
    return NULL;
}

void *popFront(struct List *list)
{
    if (isEmptyList(list))
	return NULL;

    struct Node *oldHead = list->head;
    list->head = oldHead->next;
    void *data = oldHead->data;
    free(oldHead);
    return data;
}

void removeAllNodes(struct List *list)
{
    while (!isEmptyList(list))
	popFront(list);
}

struct Node *addAfter(struct List *list,
	struct Node *prevNode, void *data)
{
    if (prevNode == NULL)
	return addFront(list, data);

    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;

    node->data = data;
    node->next = prevNode->next;
    prevNode->next = node;
    return node;
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

struct Node *addBack(struct List *list, void *data, enum fdl_type t)
{
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;
    node->data = data;
    node->type = t;
    node->next = NULL;

    if (list->head == NULL) {
	list->head = node;
	return node;
    }

    struct Node *end = list->head;
    while (end->next != NULL)
	end = end->next;

    end->next = node;
    return node;
}



#endif
