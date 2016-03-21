#include <stdio.h>
#include <stdlib.h>
#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return((pointer) lbl)
typedef int * pointer;
typedef pointer (* function)();

pointer Stack[10000];
pointer* SpB = Stack;
pointer* SpA = Stack + 9999;
pointer Heap[10000];
pointer* Hp = Heap + 9999;
pointer* HLimit = Heap;
pointer Node;
int main();
pointer main_1_1_entry();
pointer main_1_entry();
pointer main_2_entry();
pointer main_entry();
pointer caseList_entry();
pointer constFalse_entry();
pointer isNil_2_entry();
pointer isNil_entry();
pointer if_entry();
pointer id_entry();
pointer map_entry();
pointer mf_entry();
pointer map1_entry();
pointer test_entry();
pointer apply3_entry();
pointer plus_entry();
pointer compose_1_entry();
pointer compose_entry();
pointer c_1_entry();
pointer c_entry();
pointer f_entry();
pointer g_entry();
pointer h_entry();
pointer testLetRec_entry();
pointer ff_entry();
pointer id3_entry();
pointer main_info[] = {(pointer)main_entry};
pointer caseList_info[] = {(pointer)caseList_entry};
pointer isNil_info[] = {(pointer)isNil_entry};
pointer if_info[] = {(pointer)if_entry};
pointer id_info[] = {(pointer)id_entry};
pointer map_info[] = {(pointer)map_entry};
pointer map1_info[] = {(pointer)map1_entry};
pointer test_info[] = {(pointer)test_entry};
pointer apply3_info[] = {(pointer)apply3_entry};
pointer plus_info[] = {(pointer)plus_entry};
pointer compose_info[] = {(pointer)compose_entry};
pointer testLetRec_info[] = {(pointer)testLetRec_entry};
pointer id3_info[] = {(pointer)id3_entry};
pointer main_closure[] = {(pointer)main_info};
pointer caseList_closure[] = {(pointer)caseList_info};
pointer isNil_closure[] = {(pointer)isNil_info};
pointer if_closure[] = {(pointer)if_info};
pointer id_closure[] = {(pointer)id_info};
pointer map_closure[] = {(pointer)map_info};
pointer map1_closure[] = {(pointer)map1_info};
pointer test_closure[] = {(pointer)test_info};
pointer apply3_closure[] = {(pointer)apply3_info};
pointer plus_closure[] = {(pointer)plus_info};
pointer compose_closure[] = {(pointer)compose_info};
pointer testLetRec_closure[] = {(pointer)testLetRec_info};
pointer id3_closure[] = {(pointer)id3_info};
pointer main_1_1_info[] = {(pointer)main_1_1_entry};
pointer main_1_info[] = {(pointer)main_1_entry};
pointer main_2_info[] = {(pointer)main_2_entry};
pointer constFalse_info[] = {(pointer)constFalse_entry};
pointer isNil_2_info[] = {(pointer)isNil_2_entry};
pointer mf_info[] = {(pointer)mf_entry};
pointer compose_1_info[] = {(pointer)compose_1_entry};
pointer c_1_info[] = {(pointer)c_1_entry};
pointer c_info[] = {(pointer)c_entry};
pointer f_info[] = {(pointer)f_entry};
pointer g_info[] = {(pointer)g_entry};
pointer h_info[] = {(pointer)h_entry};
pointer ff_info[] = {(pointer)ff_entry};

pointer main_1_1_entry() {
    JUMP(main);
}

pointer main_1_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for main_1_1 */
    Hp[0] = (pointer)main_1_1_info;
    /* Evaluate body */
    SpA[-1] = (pointer)Hp;         /* Push main_1_1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer)map_closure;   /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer main_2_entry() {
    JUMP(main);
}

pointer main_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for main_1 */
    Hp[0] = (pointer)main_1_info;
    /* Fill in closure for main_2 */
    Hp[1] = (pointer)main_2_info;
    /* Evaluate body */
    SpA[-1] = (pointer)id_closure; /* Push id onto stack */
    SpA[-2] = (pointer)Hp + 1;     /* Push main_2 onto stack */
    SpA[-3] = (pointer)Hp;         /* Push main_1 onto stack */
    SpA = SpA - 3;                 /* Adjust SpA */
    Node = (pointer)if_closure;    /* Grab if into Node */
    ENTER((pointer**)Node);        /* Enter if */
}

pointer caseList_entry() {
    JUMP(main);
}

pointer constFalse_entry() {
    JUMP(main);
}

pointer isNil_2_entry() {
    JUMP(main);
}

pointer isNil_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for constFalse */
    Hp[0] = (pointer)constFalse_info;
    /* Evaluate body */
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for isNil_2 */
    Hp[0] = (pointer)isNil_2_info;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab list into a local variable */
    SpA[0] = (pointer)Hp + 1;      /* Push constFalse onto stack */
    SpA[-1] = (pointer)Hp;         /* Push isNil_2 onto stack */
    SpA[-2] = t0;                  /* Push list onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer)caseList_closure; /* Grab caseList into Node */
    ENTER((pointer**)Node);        /* Enter caseList */
}

pointer if_entry() {
    JUMP(main);
}

pointer id_entry() {
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = t0;                     /* Grab x into Node */
    ENTER((pointer**)Node);        /* Enter x */
}

pointer map_entry() {
    JUMP(main);
}

pointer mf_entry() {
    JUMP(main);
}

pointer map1_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for mf */
    Hp[0] = (pointer)mf_info;
    Hp[1] = SpA[0];                /* f */
    Hp[2] = (pointer)Hp;           /* mf */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer)Hp;            /* Grab mf into Node */
    ENTER((pointer**)Node);        /* Enter mf */
}

pointer test_entry() {
    JUMP(main);
}

pointer apply3_entry() {
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    pointer t1 = SpA[1];           /* Grab x into a local variable */
    SpA[1] = t1;                   /* Push x onto stack */
    SpA[0] = t1;                   /* Push x onto stack */
    SpA[-1] = t1;                  /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = t0;                     /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer plus_entry() {
    JUMP(main);
}

pointer compose_1_entry() {
    SpA[-1] = Node + 2;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer compose_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for compose_1 */
    Hp[0] = (pointer)compose_1_info;
    Hp[1] = SpA[1];                /* g */
    Hp[2] = SpA[2];                /* x */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    pointer t1 = SpA[1];           /* Grab g into a local variable */
    pointer t2 = SpA[2];           /* Grab x into a local variable */
    SpA[2] = (pointer)Hp;          /* Push compose_1 onto stack */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = t0;                     /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer c_1_entry() {
    SpA[-1] = Node + 2;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer c_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for c_1 */
    Hp[0] = (pointer)c_1_info;
    Hp[1] = Node + 1;              /* g */
    Hp[2] = Node + 3;              /* x */
    /* Evaluate body */
    SpA[-1] = (pointer)Hp;         /* Push c_1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 2;               /* Grab h into Node */
    ENTER((pointer**)Node);        /* Enter h */
}

pointer f_entry() {
    Hp = Hp - 4;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for c */
    Hp[0] = (pointer)c_info;
    Hp[1] = Node + 1;              /* g */
    Hp[2] = Node + 2;              /* h */
    Hp[3] = Node + 3;              /* x */
    /* Evaluate body */
    SpA[-1] = Node + 2;            /* Push h onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer g_entry() {
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA[0] = t0;                   /* Push x onto stack */
    Node = Node + 1;               /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer h_entry() {
    SpA[-1] = Node + 2;            /* Push g onto stack */
    SpA[-2] = Node + 1;            /* Push f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = Node + 3;               /* Grab h into Node */
    ENTER((pointer**)Node);        /* Enter h */
}

pointer testLetRec_entry() {
    Hp = Hp - 10;                  /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for f */
    Hp[0] = (pointer)f_info;
    Hp[1] = (pointer)Hp + 4;       /* g */
    Hp[2] = (pointer)Hp + 6;       /* h */
    Hp[3] = Node + 1;              /* x */
    /* Fill in closure for g */
    Hp[4] = (pointer)g_info;
    Hp[5] = (pointer)Hp;           /* f */
    /* Fill in closure for h */
    Hp[6] = (pointer)h_info;
    Hp[7] = (pointer)Hp;           /* f */
    Hp[8] = (pointer)Hp + 4;       /* g */
    Hp[9] = (pointer)Hp + 6;       /* h */
    /* Evaluate body */
    SpA[-1] = Node + 1;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer)Hp + 4;        /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer ff_entry() {
    JUMP(main);
}

pointer id3_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    } 
    /* Fill in closure for ff */
    Hp[0] = (pointer)ff_info;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    pointer t1 = SpA[1];           /* Grab y into a local variable */
    pointer t2 = SpA[2];           /* Grab z into a local variable */
    SpA[2] = t2;                   /* Push z onto stack */
    SpA[1] = t1;                   /* Push y onto stack */
    SpA[0] = t0;                   /* Push x onto stack */
    Node = (pointer)testLetRec_closure; /* Grab testLetRec into Node */
    ENTER((pointer**)Node);        /* Enter testLetRec */
}

int main() {
    function f_main = (function)main;
    function cont = main_entry;
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}