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
pointer* Node;
int RTag;
int IntReg;
int main();
pointer MkFloat_entry();
pointer MkInt_entry();
pointer Just_entry();
pointer Nothing_entry();
pointer False_entry();
pointer True_entry();
pointer Nil_entry();
pointer Cons_entry();
pointer main_t1_entry();
pointer main_t2_entry();
pointer main_entry();
pointer alt1();
pointer caseList_entry();
pointer isNil_constFalse_entry();
pointer isNil_t2_entry();
pointer isNil_entry();
pointer alt2();
pointer if_entry();
pointer id_entry();
pointer map_t1_entry();
pointer map_t2_entry();
pointer alt3();
pointer map_entry();
pointer map1_mf_t1_entry();
pointer map1_mf_t2_entry();
pointer alt4();
pointer map1_mf_entry();
pointer map1_entry();
pointer test_blah_entry();
pointer alt5();
pointer test_entry();
pointer apply3_entry();
pointer alt8();
pointer alt7();
pointer alt6();
pointer plus_entry();
pointer compose_t1_entry();
pointer compose_entry();
pointer testLR_f_c_t1_entry();
pointer testLR_f_c_entry();
pointer testLR_f_entry();
pointer testLR_g_entry();
pointer testLR_h_entry();
pointer testLR_entry();
pointer testBoxed_entry();
pointer testBoxed_info[] = {(pointer)testBoxed_entry};
pointer testLR_info[] = {(pointer)testLR_entry};
pointer testLR_h_info[] = {(pointer)testLR_h_entry};
pointer testLR_g_info[] = {(pointer)testLR_g_entry};
pointer testLR_f_info[] = {(pointer)testLR_f_entry};
pointer testLR_f_c_info[] = {(pointer)testLR_f_c_entry};
pointer testLR_f_c_t1_info[] = {(pointer)testLR_f_c_t1_entry};
pointer compose_info[] = {(pointer)compose_entry};
pointer compose_t1_info[] = {(pointer)compose_t1_entry};
pointer plus_info[] = {(pointer)plus_entry};
pointer apply3_info[] = {(pointer)apply3_entry};
pointer test_info[] = {(pointer)test_entry};
pointer test_blah_info[] = {(pointer)test_blah_entry};
pointer map1_info[] = {(pointer)map1_entry};
pointer map1_mf_info[] = {(pointer)map1_mf_entry};
pointer map1_mf_t2_info[] = {(pointer)map1_mf_t2_entry};
pointer map1_mf_t1_info[] = {(pointer)map1_mf_t1_entry};
pointer map_info[] = {(pointer)map_entry};
pointer map_t2_info[] = {(pointer)map_t2_entry};
pointer map_t1_info[] = {(pointer)map_t1_entry};
pointer id_info[] = {(pointer)id_entry};
pointer if_info[] = {(pointer)if_entry};
pointer isNil_info[] = {(pointer)isNil_entry};
pointer isNil_t2_info[] = {(pointer)isNil_t2_entry};
pointer isNil_constFalse_info[] = {(pointer)isNil_constFalse_entry};
pointer caseList_info[] = {(pointer)caseList_entry};
pointer main_info[] = {(pointer)main_entry};
pointer main_t2_info[] = {(pointer)main_t2_entry};
pointer main_t1_info[] = {(pointer)main_t1_entry};
pointer MkFloat_info[] = {(pointer)MkFloat_entry};
pointer MkInt_info[] = {(pointer)MkInt_entry};
pointer Just_info[] = {(pointer)Just_entry};
pointer Nothing_info[] = {(pointer)Nothing_entry};
pointer False_info[] = {(pointer)False_entry};
pointer True_info[] = {(pointer)True_entry};
pointer Nil_info[] = {(pointer)Nil_entry};
pointer Cons_info[] = {(pointer)Cons_entry};
pointer testBoxed_closure[] = {(pointer)testBoxed_info};
pointer testLR_closure[] = {(pointer)testLR_info};
pointer compose_closure[] = {(pointer)compose_info};
pointer plus_closure[] = {(pointer)plus_info};
pointer apply3_closure[] = {(pointer)apply3_info};
pointer test_closure[] = {(pointer)test_info};
pointer map1_closure[] = {(pointer)map1_info};
pointer map_closure[] = {(pointer)map_info};
pointer id_closure[] = {(pointer)id_info};
pointer if_closure[] = {(pointer)if_info};
pointer isNil_closure[] = {(pointer)isNil_info};
pointer caseList_closure[] = {(pointer)caseList_info};
pointer main_closure[] = {(pointer)main_info};

pointer MkFloat_entry() {
    printf("MkFloat\n");
    RTag = 8;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkInt_entry() {
    printf("MkInt %d\n", (int)Node[1]);
    RTag = 7;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Just_entry() {
    printf("Just\n");
    RTag = 6;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Nothing_entry() {
    printf("Nothing\n");
    RTag = 5;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer False_entry() {
    printf("False\n");
    RTag = 4;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer True_entry() {
    printf("True\n");
    RTag = 3;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Nil_entry() {
    printf("Nil\n");
    RTag = 2;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Cons_entry() {
    printf("Cons\n");
    RTag = 1;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer main_t1_entry() {
    printf("main_t1\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)(MkInt_info);
    Hp[1] = (pointer)(2);          /* 2 */
    Node = (pointer*)(Hp);         /* Grab MkInt into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
}

pointer main_t2_entry() {
    printf("main_t2\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)(MkInt_info);
    Hp[1] = (pointer)(3);          /* 3 */
    Node = (pointer*)(Hp);         /* Grab MkInt into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
}

pointer main_entry() {
    printf("main\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_t1 */
    Hp[0] = (pointer)(main_t1_info);
    /* Fill in closure for main_t2 */
    Hp[1] = (pointer)(main_t2_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp + 1);   /* Push main_t2 onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push main_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(plus_closure); /* Grab plus into Node */
    ENTER((pointer**)Node);        /* Enter plus */
}

pointer alt1() {
    printf("alt1\n");
    switch (RTag) {
        case 2:
        {
            pointer a0 = SpA[0];           /* Grab list into a local variable */
            pointer a1 = SpA[1];           /* Grab f into a local variable */
            pointer a2 = SpA[2];           /* Grab g into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(a1);         /* Grab f into Node */
            ENTER((pointer**)Node);        /* Enter f */
            break;
        }
        case 1:
        {
            pointer a0 = SpA[0];           /* Grab list into a local variable */
            pointer a1 = SpA[1];           /* Grab f into a local variable */
            pointer a2 = SpA[2];           /* Grab g into a local variable */
            SpA[2] = (pointer)(Node + 2);  /* Push tail onto stack */
            SpA[1] = (pointer)(Node + 1);  /* Push head onto stack */
            SpA = SpA + 1;                 /* Adjust SpA */
            Node = (pointer*)(a2);         /* Grab g into Node */
            ENTER((pointer**)Node);        /* Enter g */
            break;
        }
    }
    JUMP(main);
}

pointer caseList_entry() {
    printf("caseList\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt1);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab list into a local variable */
    pointer a1 = SpA[1];           /* Grab f into a local variable */
    pointer a2 = SpA[2];           /* Grab g into a local variable */
    Node = (pointer*)(a0);         /* Grab list into Node */
    ENTER((pointer**)Node);        /* Enter list */
}

pointer isNil_constFalse_entry() {
    printf("isNil_constFalse\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for False */
    Hp[0] = (pointer)(False_info);
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab False into Node */
    ENTER((pointer**)Node);        /* Enter False */
}

pointer isNil_t2_entry() {
    printf("isNil_t2\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for True */
    Hp[0] = (pointer)(True_info);
    Node = (pointer*)(Hp);         /* Grab True into Node */
    ENTER((pointer**)Node);        /* Enter True */
}

pointer isNil_entry() {
    printf("isNil\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for isNil_constFalse */
    Hp[0] = (pointer)(isNil_constFalse_info);
    /* Evaluate body */
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for isNil_t2 */
    Hp[0] = (pointer)(isNil_t2_info);
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab list into a local variable */
    SpA[0] = (pointer)(Hp + 1);    /* Push isNil_constFalse onto stack */
    SpA[-1] = (pointer)(Hp);       /* Push isNil_t2 onto stack */
    SpA[-2] = (pointer)(a0);       /* Push list onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(caseList_closure); /* Grab caseList into Node */
    ENTER((pointer**)Node);        /* Enter caseList */
}

pointer alt2() {
    printf("alt2\n");
    switch (RTag) {
        case 3:
        {
            pointer a0 = SpA[0];           /* Grab cond into a local variable */
            pointer a1 = SpA[1];           /* Grab then into a local variable */
            pointer a2 = SpA[2];           /* Grab else into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(a1);         /* Grab then into Node */
            ENTER((pointer**)Node);        /* Enter then */
            break;
        }
        case 4:
        {
            pointer a0 = SpA[0];           /* Grab cond into a local variable */
            pointer a1 = SpA[1];           /* Grab then into a local variable */
            pointer a2 = SpA[2];           /* Grab else into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(a2);         /* Grab else into Node */
            ENTER((pointer**)Node);        /* Enter else */
            break;
        }
    }
    JUMP(main);
}

pointer if_entry() {
    printf("if\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt2);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab cond into a local variable */
    pointer a1 = SpA[1];           /* Grab then into a local variable */
    pointer a2 = SpA[2];           /* Grab else into a local variable */
    Node = (pointer*)(a0);         /* Grab cond into Node */
    ENTER((pointer**)Node);        /* Enter cond */
}

pointer id_entry() {
    printf("id\n");
    pointer a0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab x into Node */
    ENTER((pointer**)Node);        /* Enter x */
}

pointer map_t1_entry() {
    printf("map_t1\n");
    SpA[-1] = (pointer)(Node + 2); /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map_t2_entry() {
    printf("map_t2\n");
    SpA[-1] = (pointer)(Node + 2); /* Push ys onto stack */
    SpA[-2] = (pointer)(Node + 1); /* Push f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(map_closure); /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer alt3() {
    printf("alt3\n");
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)(Nil_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab Nil into Node */
            ENTER((pointer**)Node);        /* Enter Nil */
            break;
        }
        case 1:
        {
            Hp = Hp - 6;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for map_t1 */
            Hp[0] = (pointer)(map_t1_info);
            Hp[1] = (pointer)(SpA[0]);     /* f */
            Hp[2] = (pointer)(Node + 1);   /* y */
            /* Fill in closure for map_t2 */
            Hp[3] = (pointer)(map_t2_info);
            Hp[4] = (pointer)(SpA[0]);     /* f */
            Hp[5] = (pointer)(Node + 2);   /* ys */
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Cons */
            Hp[0] = (pointer)(Cons_info);
            Hp[1] = (pointer)(Hp + 3);     /* map_t1 */
            Hp[2] = (pointer)(Hp + 6);     /* map_t2 */
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab Cons into Node */
            ENTER((pointer**)Node);        /* Enter Cons */
            break;
        }
    }
    JUMP(main);
}

pointer map_entry() {
    printf("map\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt3);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    pointer a1 = SpA[1];           /* Grab xs into a local variable */
    Node = (pointer*)(a1);         /* Grab xs into Node */
    ENTER((pointer**)Node);        /* Enter xs */
}

pointer map1_mf_t1_entry() {
    printf("map1_mf_t1\n");
    SpA[-1] = (pointer)(Node + 2); /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map1_mf_t2_entry() {
    printf("map1_mf_t2\n");
    SpA[-1] = (pointer)(Node + 2); /* Push ys onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab map1_mf into Node */
    ENTER((pointer**)Node);        /* Enter map1_mf */
}

pointer alt4() {
    printf("alt4\n");
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)(Nil_info);
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab Nil into Node */
            ENTER((pointer**)Node);        /* Enter Nil */
            break;
        }
        case 1:
        {
            Hp = Hp - 6;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for map1_mf_t1 */
            Hp[0] = (pointer)(map1_mf_t1_info);
            Hp[1] = (pointer)(SpA[1]);     /* f */
            Hp[2] = (pointer)(Node + 1);   /* y */
            /* Fill in closure for map1_mf_t2 */
            Hp[3] = (pointer)(map1_mf_t2_info);
            Hp[4] = (pointer)(SpA[0]);     /* map1_mf */
            Hp[5] = (pointer)(Node + 2);   /* ys */
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Cons */
            Hp[0] = (pointer)(Cons_info);
            Hp[1] = (pointer)(Hp + 3);     /* map1_mf_t1 */
            Hp[2] = (pointer)(Hp + 6);     /* map1_mf_t2 */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab Cons into Node */
            ENTER((pointer**)Node);        /* Enter Cons */
            break;
        }
    }
    JUMP(main);
}

pointer map1_mf_entry() {
    printf("map1_mf\n");
    /* Save local environment */
    SpA[-1] = (pointer)(Node + 1); /* Save f */
    SpA[-2] = (pointer)(Node + 2); /* Save map1_mf */
    SpA = SpA - 2;                 /* Adjust SpA */
    /* Push return address */
    SpB[1] = (pointer)(alt4);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a2 = SpA[2];           /* Grab xs into a local variable */
    pointer a1 = SpA[1];           /* Grab f into a local variable */
    pointer a0 = SpA[0];           /* Grab map1_mf into a local variable */
    Node = (pointer*)(a2);         /* Grab xs into Node */
    ENTER((pointer**)Node);        /* Enter xs */
}

pointer map1_entry() {
    printf("map1\n");
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for map1_mf */
    Hp[0] = (pointer)(map1_mf_info);
    Hp[1] = (pointer)(SpA[0]);     /* f */
    Hp[2] = (pointer)(Hp);         /* map1_mf */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab map1_mf into Node */
    ENTER((pointer**)Node);        /* Enter map1_mf */
}

pointer test_blah_entry() {
    printf("test_blah\n");
    Node = (pointer*)(Node + 1);   /* Grab list into Node */
    ENTER((pointer**)Node);        /* Enter list */
}

pointer alt5() {
    printf("alt5\n");
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)(Nil_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab Nil into Node */
            ENTER((pointer**)Node);        /* Enter Nil */
            break;
        }
        default:
        {
            pointer a1 = SpA[1];           /* Grab list into a local variable */
            pointer a0 = SpA[0];           /* Grab test_blah into a local variable */
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = (pointer*)(a1);         /* Grab list into Node */
            ENTER((pointer**)Node);        /* Enter list */
            break;
        }
    }
    JUMP(main);
}

pointer test_entry() {
    printf("test\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for test_blah */
    Hp[0] = (pointer)(test_blah_info);
    Hp[1] = (pointer)(SpA[0]);     /* list */
    /* Evaluate body */
    /* Save local environment */
    SpA[-1] = (pointer)(Hp);       /* Save test_blah */
    SpA = SpA - 1;                 /* Adjust SpA */
    /* Push return address */
    SpB[1] = (pointer)(alt5);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a1 = SpA[1];           /* Grab list into a local variable */
    pointer a0 = SpA[0];           /* Grab test_blah into a local variable */
    Node = (pointer*)(a0);         /* Grab test_blah into Node */
    ENTER((pointer**)Node);        /* Enter test_blah */
}

pointer apply3_entry() {
    printf("apply3\n");
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    pointer a1 = SpA[1];           /* Grab x into a local variable */
    SpA[1] = (pointer)(a1);        /* Push x onto stack */
    SpA[0] = (pointer)(a1);        /* Push x onto stack */
    SpA[-1] = (pointer)(a1);       /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer alt8() {
    printf("alt8\n");
    switch (IntReg) {
        default:
        {
            int t$ = IntReg;
            Hp = Hp - 2;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for MkInt */
            Hp[0] = (pointer)(MkInt_info);
            Hp[1] = (pointer)(t$);         /* t$ */
            SpA = SpA + 2;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab MkInt into Node */
            ENTER((pointer**)Node);        /* Enter MkInt */
            break;
        }
    }
    JUMP(main);
}

pointer alt7() {
    printf("alt7\n");
    switch (RTag) {
        case 7:
        {
            /* Save local environment */
            SpB[1] = (pointer)(*(Node + 1)); /* Save y$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt8);
            SpB = SpB + 1;
            /* Evaluate body */
            IntReg = (int)SpB[-2] + (int)SpB[-1];
            SpB = SpB - 1;
            JUMP(SpB[1]);                  /* Enter return address */
            break;
        }
    }
    JUMP(main);
}

pointer alt6() {
    printf("alt6\n");
    switch (RTag) {
        case 7:
        {
            /* Save local environment */
            SpB[1] = (pointer)(*(Node + 1)); /* Save x$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt7);
            SpB = SpB + 1;
            /* Evaluate body */
            pointer a0 = SpA[0];           /* Grab e1 into a local variable */
            pointer a1 = SpA[1];           /* Grab e2 into a local variable */
            pointer b1 = SpB[-1];          /* Grab x$ into a local variable */
            Node = (pointer*)(a1);         /* Grab e2 into Node */
            ENTER((pointer**)Node);        /* Enter e2 */
            break;
        }
    }
    JUMP(main);
}

pointer plus_entry() {
    printf("plus\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt6);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab e1 into a local variable */
    pointer a1 = SpA[1];           /* Grab e2 into a local variable */
    Node = (pointer*)(a0);         /* Grab e1 into Node */
    ENTER((pointer**)Node);        /* Enter e1 */
}

pointer compose_t1_entry() {
    printf("compose_t1\n");
    SpA[-1] = (pointer)(Node + 2); /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer compose_entry() {
    printf("compose\n");
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for compose_t1 */
    Hp[0] = (pointer)(compose_t1_info);
    Hp[1] = (pointer)(SpA[1]);     /* g */
    Hp[2] = (pointer)(SpA[2]);     /* x */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    pointer a1 = SpA[1];           /* Grab g into a local variable */
    pointer a2 = SpA[2];           /* Grab x into a local variable */
    SpA[2] = (pointer)(Hp);        /* Push compose_t1 onto stack */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer testLR_f_c_t1_entry() {
    printf("testLR_f_c_t1\n");
    SpA[-1] = (pointer)(Node + 2); /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer testLR_f_c_entry() {
    printf("testLR_f_c\n");
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f_c_t1 */
    Hp[0] = (pointer)(testLR_f_c_t1_info);
    Hp[1] = (pointer)(Node + 1);   /* testLR_g */
    Hp[2] = (pointer)(Node + 3);   /* x */
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push testLR_f_c_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 2);   /* Grab testLR_h into Node */
    ENTER((pointer**)Node);        /* Enter testLR_h */
}

pointer testLR_f_entry() {
    printf("testLR_f\n");
    Hp = Hp - 4;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f_c */
    Hp[0] = (pointer)(testLR_f_c_info);
    Hp[1] = (pointer)(Node + 1);   /* testLR_g */
    Hp[2] = (pointer)(Node + 2);   /* testLR_h */
    Hp[3] = (pointer)(Node + 3);   /* x */
    /* Evaluate body */
    SpA[-1] = (pointer)(Node + 2); /* Push testLR_h onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node + 1);   /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer testLR_g_entry() {
    printf("testLR_g\n");
    pointer a0 = SpA[0];           /* Grab x into a local variable */
    SpA[0] = (pointer)(a0);        /* Push x onto stack */
    Node = (pointer*)(Node + 1);   /* Grab testLR_f into Node */
    ENTER((pointer**)Node);        /* Enter testLR_f */
}

pointer testLR_h_entry() {
    printf("testLR_h\n");
    SpA[-1] = (pointer)(Node + 2); /* Push testLR_g onto stack */
    SpA[-2] = (pointer)(Node + 1); /* Push testLR_f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(Node + 3);   /* Grab testLR_h into Node */
    ENTER((pointer**)Node);        /* Enter testLR_h */
}

pointer testLR_entry() {
    printf("testLR\n");
    Hp = Hp - 10;                  /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for testLR_f */
    Hp[0] = (pointer)(testLR_f_info);
    Hp[1] = (pointer)(Hp + 4);     /* testLR_g */
    Hp[2] = (pointer)(Hp + 6);     /* testLR_h */
    Hp[3] = (pointer)(SpA[0]);     /* x */
    /* Fill in closure for testLR_g */
    Hp[4] = (pointer)(testLR_g_info);
    Hp[5] = (pointer)(Hp);         /* testLR_f */
    /* Fill in closure for testLR_h */
    Hp[6] = (pointer)(testLR_h_info);
    Hp[7] = (pointer)(Hp);         /* testLR_f */
    Hp[8] = (pointer)(Hp + 4);     /* testLR_g */
    Hp[9] = (pointer)(Hp + 6);     /* testLR_h */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab x into a local variable */
    SpA[0] = (pointer)(a0);        /* Push x onto stack */
    Node = (pointer*)(Hp + 4);     /* Grab testLR_g into Node */
    ENTER((pointer**)Node);        /* Enter testLR_g */
}

pointer testBoxed_entry() {
    printf("testBoxed\n");
    pointer a0 = SpA[0];           /* Grab z into a local variable */
    pointer b0 = SpB[0];           /* Grab x$ into a local variable */
    pointer b1 = SpB[-1];          /* Grab y$ into a local variable */
    SpB[-1] = (pointer)(b0);       /* Push x$ onto stack */
    SpB[0] = (pointer)(b1);        /* Push y$ onto stack */
    SpB[1] = (pointer)(44);        /* Push 44 onto stack */
    SpB[2] = (pointer)(b0);        /* Push x$ onto stack */
    SpA[0] = (pointer)(a0);        /* Push z onto stack */
    SpB = SpB + 2;                 /* Adjust SpB */
    Node = (pointer*)(Node + 1);   /* Grab hello into Node */
    ENTER((pointer**)Node);        /* Enter hello */
}

int main() {
    function f_main = (function)main;
    function cont = main_entry;
    SpB[0] = (pointer)((pointer)f_main); /* Push return address */
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}
