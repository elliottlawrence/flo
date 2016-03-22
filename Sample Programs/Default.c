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
pointer main_t1_t1_entry();
pointer main_t1_entry();
pointer main_t2_entry();
pointer main_entry();
pointer caseList_entry();
pointer isNil_constFalse_entry();
pointer isNil_t2_entry();
pointer isNil_entry();
pointer if_entry();
pointer id_entry();
pointer map_entry();
pointer map1_mf_entry();
pointer map1_entry();
pointer test_entry();
pointer apply3_entry();
pointer plus_entry();
pointer compose_t1_entry();
pointer compose_entry();
pointer testLR_f_c_t1_entry();
pointer testLR_f_c_entry();
pointer testLR_f_entry();
pointer testLR_g_entry();
pointer testLR_h_entry();
pointer testLR_entry();
pointer id3_f_t0_entry();
pointer id3_f_entry();
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
pointer testLR_info[] = {(pointer)testLR_entry};
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
pointer testLR_closure[] = {(pointer)testLR_info};
pointer id3_closure[] = {(pointer)id3_info};
pointer main_t1_t1_info[] = {(pointer)main_t1_t1_entry};
pointer main_t1_info[] = {(pointer)main_t1_entry};
pointer main_t2_info[] = {(pointer)main_t2_entry};
pointer isNil_constFalse_info[] = {(pointer)isNil_constFalse_entry};
pointer isNil_t2_info[] = {(pointer)isNil_t2_entry};
pointer map1_mf_info[] = {(pointer)map1_mf_entry};
pointer compose_t1_info[] = {(pointer)compose_t1_entry};
pointer testLR_f_c_t1_info[] = {(pointer)testLR_f_c_t1_entry};
pointer testLR_f_c_info[] = {(pointer)testLR_f_c_entry};
pointer testLR_f_info[] = {(pointer)testLR_f_entry};
pointer testLR_g_info[] = {(pointer)testLR_g_entry};
pointer testLR_h_info[] = {(pointer)testLR_h_entry};
pointer id3_f_t0_info[] = {(pointer)id3_f_t0_entry};
pointer id3_f_info[] = {(pointer)id3_f_entry};

pointer main_t1_t1_entry() {
    JUMP(main);
}

pointer main_t1_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_t1_t1 */
    Hp[0] = (pointer)main_t1_t1_info;
    /* Evaluate body */
    SpA[-1] = (pointer)Hp;         /* Push main_t1_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer)map_closure;   /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer main_t2_entry() {
    JUMP(main);
}

pointer main_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_t1 */
    Hp[0] = (pointer)main_t1_info;
    /* Fill in closure for main_t2 */
    Hp[1] = (pointer)main_t2_info;
    /* Evaluate body */
    SpA[-1] = (pointer)id_closure; /* Push id onto stack */
    SpA[-2] = (pointer)Hp + 1;     /* Push main_t2 onto stack */
    SpA[-3] = (pointer)Hp;         /* Push main_t1 onto stack */
    SpA = SpA - 3;                 /* Adjust SpA */
    Node = (pointer)if_closure;    /* Grab if into Node */
    ENTER((pointer**)Node);        /* Enter if */
}

pointer caseList_entry() {
    JUMP(main);
}

pointer isNil_constFalse_entry() {
    JUMP(main);
}

pointer isNil_t2_entry() {
    JUMP(main);
}

pointer isNil_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for isNil_constFalse */
    Hp[0] = (pointer)isNil_constFalse_info;
    /* Evaluate body */
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for isNil_t2 */
    Hp[0] = (pointer)isNil_t2_info;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab list into a local variable */
    SpA[0] = (pointer)Hp + 1;      /* Push isNil_constFalse onto stack */
    SpA[-1] = (pointer)Hp;         /* Push isNil_t2 onto stack */
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

pointer map1_mf_entry() {
    JUMP(main);
}

pointer map1_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for map1_mf */
    Hp[0] = (pointer)map1_mf_info;
    Hp[1] = SpA[0];                /* f */
    Hp[2] = (pointer)Hp;           /* map1_mf */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer)Hp;            /* Grab map1_mf into Node */
    ENTER((pointer**)Node);        /* Enter map1_mf */
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

pointer compose_t1_entry() {
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
    /* Fill in closure for compose_t1 */
    Hp[0] = (pointer)compose_t1_info;
    Hp[1] = SpA[1];                /* g */
    Hp[2] = SpA[2];                /* x */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    pointer t1 = SpA[1];           /* Grab g into a local variable */
    pointer t2 = SpA[2];           /* Grab x into a local variable */
    SpA[2] = (pointer)Hp;          /* Push compose_t1 onto stack */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = t0;                     /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer testLR_f_c_t1_entry() {
    SpA[-1] = Node + 2;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer testLR_f_c_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f_c_t1 */
    Hp[0] = (pointer)testLR_f_c_t1_info;
    Hp[1] = Node + 1;              /* testLR_g */
    Hp[2] = Node + 3;              /* x */
    /* Evaluate body */
    SpA[-1] = (pointer)Hp;         /* Push testLR_f_c_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 2;               /* Grab testLR_h into Node */
    ENTER((pointer**)Node);        /* Enter testLR_h */
}

pointer testLR_f_entry() {
    Hp = Hp - 4;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f_c */
    Hp[0] = (pointer)testLR_f_c_info;
    Hp[1] = Node + 1;              /* testLR_g */
    Hp[2] = Node + 2;              /* testLR_h */
    Hp[3] = Node + 3;              /* x */
    /* Evaluate body */
    SpA[-1] = Node + 2;            /* Push testLR_h onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer testLR_g_entry() {
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA[0] = t0;                   /* Push x onto stack */
    Node = Node + 1;               /* Grab testLR_f into Node */
    ENTER((pointer**)Node);        /* Enter testLR_f */
}

pointer testLR_h_entry() {
    SpA[-1] = Node + 2;            /* Push testLR_g onto stack */
    SpA[-2] = Node + 1;            /* Push testLR_f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = Node + 3;               /* Grab testLR_h into Node */
    ENTER((pointer**)Node);        /* Enter testLR_h */
}

pointer testLR_entry() {
    Hp = Hp - 10;                  /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f */
    Hp[0] = (pointer)testLR_f_info;
    Hp[1] = (pointer)Hp + 4;       /* testLR_g */
    Hp[2] = (pointer)Hp + 6;       /* testLR_h */
    Hp[3] = Node + 1;              /* x */
    /* Fill in closure for testLR_g */
    Hp[4] = (pointer)testLR_g_info;
    Hp[5] = (pointer)Hp;           /* testLR_f */
    /* Fill in closure for testLR_h */
    Hp[6] = (pointer)testLR_h_info;
    Hp[7] = (pointer)Hp;           /* testLR_f */
    Hp[8] = (pointer)Hp + 4;       /* testLR_g */
    Hp[9] = (pointer)Hp + 6;       /* testLR_h */
    /* Evaluate body */
    SpA[-1] = Node + 1;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer)Hp + 4;        /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer id3_f_t0_entry() {
    pointer t0 = SpA[0];           /* Grab input2 into a local variable */
    SpA[0] = t0;                   /* Push input2 onto stack */
    SpA[-1] = Node + 2;            /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab id3_f into Node */
    ENTER((pointer**)Node);        /* Enter id3_f */
}

pointer id3_f_entry() {
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for id3_f_t0 */
    Hp[0] = (pointer)id3_f_t0_info;
    Hp[1] = Node + 1;              /* id3_f */
    Hp[2] = SpA[0];                /* x */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer)Hp;            /* Grab id3_f_t0 into Node */
    ENTER((pointer**)Node);        /* Enter id3_f_t0 */
}

pointer id3_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for id3_f */
    Hp[0] = (pointer)id3_f_info;
    Hp[1] = (pointer)Hp;           /* id3_f */
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    pointer t1 = SpA[1];           /* Grab y into a local variable */
    pointer t2 = SpA[2];           /* Grab z into a local variable */
    SpA[2] = t2;                   /* Push z onto stack */
    SpA[1] = t1;                   /* Push y onto stack */
    SpA[0] = t0;                   /* Push x onto stack */
    Node = Node + 1;               /* Grab testLetRec into Node */
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
