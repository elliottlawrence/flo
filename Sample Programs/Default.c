#include <stdio.h>
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
pointer main_entry();
pointer caseList_entry();
pointer isNil_entry();
pointer if_entry();
pointer id_entry();
pointer map_entry();
pointer map1_entry();
pointer test_entry();
pointer apply3_entry();
pointer plus_entry();
pointer compose_entry();
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

pointer main_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        
    } 
    pointer main_1_entry() {
        Hp = Hp - 1;                   /* Allocate some heap */
        if (Hp < HLimit) {
            
        } 
        pointer main_1_1_entry() {
            JUMP(main);
        }
        pointer main_1_1_info[] = {(pointer)main_1_1_entry};
        /* Fill in closure for main_1_1 */
        Hp[0] = main_1_1_info;
    }
    pointer main_1_info[] = {(pointer)main_1_entry};
    /* Fill in closure for main_1 */
    Hp[0] = main_1_info;
    pointer main_2_entry() {
        JUMP(main);
    }
    pointer main_2_info[] = {(pointer)main_2_entry};
    /* Fill in closure for main_2 */
    Hp[1] = main_2_info;
}

pointer caseList_entry() {
    JUMP(main);
}

pointer isNil_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        
    } 
    pointer constFalse_entry() {
        JUMP(main);
    }
    pointer constFalse_info[] = {(pointer)constFalse_entry};
    /* Fill in closure for constFalse */
    Hp[0] = constFalse_info;
}

pointer if_entry() {
    JUMP(main);
}

pointer id_entry() {
    Node = SpA[0];                 /* Grab x into Node */
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    ENTER((pointer**)Node);        /* Enter x */
}

pointer map_entry() {
    JUMP(main);
}

pointer map1_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        
    } 
    pointer mf_entry() {
        JUMP(main);
    }
    pointer mf_info[] = {(pointer)mf_entry};
    /* Fill in closure for mf */
    Hp[0] = mf_info;
    Hp[1] = SpA[0];                /* f */
    Hp[2] = (pointer)mf_closure;   /* mf */
}

pointer test_entry() {
    JUMP(main);
}

pointer apply3_entry() {
    Node = SpA[0];                 /* Grab f into Node */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    pointer t1 = SpA[1];           /* Grab x into a local variable */
    SpA[1] = t1;                   /* Push x onto stack */
    SpA[0] = t1;                   /* Push x onto stack */
    SpA[-1] = t1;                  /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer plus_entry() {
    JUMP(main);
}

pointer compose_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        
    } 
    pointer compose_1_entry() {
        Node = Node[1];                /* Grab g into Node */
        SpA[-1] = Node[2];             /* Push x onto stack */
        SpA = SpA - 1;                 /* Adjust SpA */
        ENTER((pointer**)Node);        /* Enter g */
    }
    pointer compose_1_info[] = {(pointer)compose_1_entry};
    /* Fill in closure for compose_1 */
    Hp[0] = compose_1_info;
    Hp[1] = SpA[1];                /* g */
    Hp[2] = SpA[2];                /* x */
}

int main() {
    function f_main = (function)main;
    function cont = main_entry;
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}