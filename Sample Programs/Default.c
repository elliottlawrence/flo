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
pointer main_t1_t1_entry();
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
pointer id3_f_t0_entry();
pointer id3_f_entry();
pointer id3_entry();
pointer id3_info[] = {(pointer)id3_entry};
pointer id3_f_info[] = {(pointer)id3_f_entry};
pointer id3_f_t0_info[] = {(pointer)id3_f_t0_entry};
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
pointer main_t1_t1_info[] = {(pointer)main_t1_t1_entry};
pointer MkFloat_info[] = {(pointer)MkFloat_entry};
pointer MkInt_info[] = {(pointer)MkInt_entry};
pointer Just_info[] = {(pointer)Just_entry};
pointer Nothing_info[] = {(pointer)Nothing_entry};
pointer False_info[] = {(pointer)False_entry};
pointer True_info[] = {(pointer)True_entry};
pointer Nil_info[] = {(pointer)Nil_entry};
pointer Cons_info[] = {(pointer)Cons_entry};
pointer id3_closure[] = {(pointer)id3_info};
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
    RTag = 8;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer MkInt_entry() {
    RTag = 7;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer Just_entry() {
    RTag = 6;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer Nothing_entry() {
    RTag = 5;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer False_entry() {
    RTag = 4;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer True_entry() {
    RTag = 3;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer Nil_entry() {
    RTag = 2;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer Cons_entry() {
    RTag = 1;
    SpB = SpB - 1;
    ENTER((pointer**)SpB[1]);      /* Enter return address */
}

pointer main_t1_t1_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)MkInt_info;
    Hp[1] = (pointer)3;            /* 3 */
    Node = (pointer)Hp;            /* Grab MkInt closure into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
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
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)MkInt_info;
    Hp[1] = (pointer)2;            /* 2 */
    Node = (pointer)Hp;            /* Grab MkInt closure into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
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

pointer alt1() {
    switch (RTag) {
        case 2:
        {
            pointer t0 = SpA[0];           /* Grab list into a local variable */
            pointer t1 = SpA[1];           /* Grab f into a local variable */
            pointer t2 = SpA[2];           /* Grab g into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = t1;                     /* Grab f into Node */
            ENTER((pointer**)Node);        /* Enter f */
            break;
        }
        case 1:
        {
            pointer t0 = SpA[0];           /* Grab list into a local variable */
            pointer t1 = SpA[1];           /* Grab f into a local variable */
            pointer t2 = SpA[2];           /* Grab g into a local variable */
            SpA[2] = (pointer)tail_closure; /* Push tail onto stack */
            SpA[1] = (pointer)head_closure; /* Push head onto stack */
            SpA = SpA + 1;                 /* Adjust SpA */
            Node = t2;                     /* Grab g into Node */
            ENTER((pointer**)Node);        /* Enter g */
            break;
        }
    }
}

pointer caseList_entry() {
    /* f is on the stack */
    /* g is on the stack */
    SpB[1] = (pointer)alt1;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab list into a local variable */
    pointer t1 = SpA[1];           /* Grab f into a local variable */
    pointer t2 = SpA[2];           /* Grab g into a local variable */
    SpA = SpA + 3;                 /* Adjust SpA */
    Node = t0;                     /* Grab list into Node */
    ENTER((pointer**)Node);        /* Enter list */
}

pointer isNil_constFalse_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for False */
    Hp[0] = (pointer)False_info;
    Node = (pointer)Hp;            /* Grab False closure into Node */
    ENTER((pointer**)Node);        /* Enter False */
}

pointer isNil_t2_entry() {
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for True */
    Hp[0] = (pointer)True_info;
    Node = (pointer)Hp;            /* Grab True closure into Node */
    ENTER((pointer**)Node);        /* Enter True */
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

pointer alt2() {
    switch (RTag) {
        case 3:
        {
            pointer t0 = SpA[0];           /* Grab cond into a local variable */
            pointer t1 = SpA[1];           /* Grab then into a local variable */
            pointer t2 = SpA[2];           /* Grab else into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = t1;                     /* Grab then into Node */
            ENTER((pointer**)Node);        /* Enter then */
            break;
        }
        case 4:
        {
            pointer t0 = SpA[0];           /* Grab cond into a local variable */
            pointer t1 = SpA[1];           /* Grab then into a local variable */
            pointer t2 = SpA[2];           /* Grab else into a local variable */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = t2;                     /* Grab else into Node */
            ENTER((pointer**)Node);        /* Enter else */
            break;
        }
    }
}

pointer if_entry() {
    /* then is on the stack */
    /* else is on the stack */
    SpB[1] = (pointer)alt2;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab cond into a local variable */
    pointer t1 = SpA[1];           /* Grab then into a local variable */
    pointer t2 = SpA[2];           /* Grab else into a local variable */
    SpA = SpA + 3;                 /* Adjust SpA */
    Node = t0;                     /* Grab cond into Node */
    ENTER((pointer**)Node);        /* Enter cond */
}

pointer id_entry() {
    pointer t0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = t0;                     /* Grab x into Node */
    ENTER((pointer**)Node);        /* Enter x */
}

pointer map_t1_entry() {
    SpA[-1] = Node + 2;            /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map_t2_entry() {
    SpA[-1] = Node + 2;            /* Push ys onto stack */
    SpA[-2] = Node + 1;            /* Push f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer)map_closure;   /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer alt3() {
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)Nil_info;
            Node = (pointer)Hp;            /* Grab Nil closure into Node */
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
            Hp[0] = (pointer)map_t1_info;
            Hp[1] = SpA[0];                /* f */
            Hp[2] = (pointer)y_closure;    /* y */
            /* Fill in closure for map_t2 */
            Hp[3] = (pointer)map_t2_info;
            Hp[4] = SpA[0];                /* f */
            Hp[5] = (pointer)ys_closure;   /* ys */
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Cons */
            Hp[0] = (pointer)Cons_info;
            Hp[1] = (pointer)Hp;           /* map_t1 */
            Hp[2] = (pointer)Hp + 3;       /* map_t2 */
            Node = (pointer)Hp;            /* Grab Cons closure into Node */
            ENTER((pointer**)Node);        /* Enter Cons */
            break;
        }
    }
}

pointer map_entry() {
    /* f is on the stack */
    SpB[1] = (pointer)alt3;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab f into a local variable */
    pointer t1 = SpA[1];           /* Grab xs into a local variable */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = t1;                     /* Grab xs into Node */
    ENTER((pointer**)Node);        /* Enter xs */
}

pointer map1_mf_t1_entry() {
    SpA[-1] = Node + 2;            /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map1_mf_t2_entry() {
    SpA[-1] = Node + 2;            /* Push ys onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = Node + 1;               /* Grab map1_mf into Node */
    ENTER((pointer**)Node);        /* Enter map1_mf */
}

pointer alt4() {
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)Nil_info;
            Node = (pointer)Hp;            /* Grab Nil closure into Node */
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
            Hp[0] = (pointer)map1_mf_t1_info;
            Hp[1] = Node + 1;              /* f */
            Hp[2] = (pointer)y_closure;    /* y */
            /* Fill in closure for map1_mf_t2 */
            Hp[3] = (pointer)map1_mf_t2_info;
            Hp[4] = Node + 2;              /* map1_mf */
            Hp[5] = (pointer)ys_closure;   /* ys */
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Cons */
            Hp[0] = (pointer)Cons_info;
            Hp[1] = (pointer)Hp;           /* map1_mf_t1 */
            Hp[2] = (pointer)Hp + 3;       /* map1_mf_t2 */
            Node = (pointer)Hp;            /* Grab Cons closure into Node */
            ENTER((pointer**)Node);        /* Enter Cons */
            break;
        }
    }
}

pointer map1_mf_entry() {
    SpA[0] = Node + 1;             /* f is in the current closure */
    SpA[-1] = Node + 2;            /* map1_mf is in the current closure */
    SpB[1] = (pointer)alt4;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab xs into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = t0;                     /* Grab xs into Node */
    ENTER((pointer**)Node);        /* Enter xs */
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

pointer test_blah_entry() {
    Node = Node + 1;               /* Grab list into Node */
    ENTER((pointer**)Node);        /* Enter list */
}

pointer alt5() {
    switch (RTag) {
        case 2:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for Nil */
            Hp[0] = (pointer)Nil_info;
            Node = (pointer)Hp;            /* Grab Nil closure into Node */
            ENTER((pointer**)Node);        /* Enter Nil */
            break;
        }
        default:
        {
            pointer t0 = SpA[0];           /* Grab list into a local variable */
            SpA = SpA + 1;                 /* Adjust SpA */
            Node = t0;                     /* Grab list into Node */
            ENTER((pointer**)Node);        /* Enter list */
            break;
        }
    }
}

pointer test_entry() {
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for test_blah */
    Hp[0] = (pointer)test_blah_info;
    Hp[1] = SpA[0];                /* list */
    /* Evaluate body */
    /* list is on the stack */
    SpB[1] = (pointer)alt5;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab list into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer)Hp;            /* Grab test_blah into Node */
    ENTER((pointer**)Node);        /* Enter test_blah */
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

pointer alt8() {
    switch (IntReg) {
        default:
        {
            Hp = Hp - 2;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for MkInt */
            Hp[0] = (pointer)MkInt_info;
            Hp[1] = (pointer)t#_closure;   /* t# */
            Node = (pointer)Hp;            /* Grab MkInt closure into Node */
            ENTER((pointer**)Node);        /* Enter MkInt */
            break;
        }
    }
}

pointer alt7() {
    switch (RTag) {
        case 7:
        {
            SpB[1] = (pointer)alt8;
            SpB = SpB + 1;
            /* Evaluate body */
            IntReg = x#_closure + y#_closure;
            SpB = SpB - 1;
            ENTER((pointer**)SpB[1]);      /* Enter return address */
            break;
        }
    }
}

pointer alt6() {
    switch (RTag) {
        case 7:
        {
            /* x# is a global */
            SpB[1] = (pointer)alt7;
            SpB = SpB + 1;
            /* Evaluate body */
            pointer t0 = SpA[0];           /* Grab e1 into a local variable */
            pointer t1 = SpA[1];           /* Grab e2 into a local variable */
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = t1;                     /* Grab e2 into Node */
            ENTER((pointer**)Node);        /* Enter e2 */
            break;
        }
    }
}

pointer plus_entry() {
    /* e2 is on the stack */
    SpB[1] = (pointer)alt6;
    SpB = SpB + 1;
    /* Evaluate body */
    pointer t0 = SpA[0];           /* Grab e1 into a local variable */
    pointer t1 = SpA[1];           /* Grab e2 into a local variable */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = t0;                     /* Grab e1 into Node */
    ENTER((pointer**)Node);        /* Enter e1 */
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
