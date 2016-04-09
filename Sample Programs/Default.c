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
intptr_t RTag;
intptr_t IntReg;
int main();
pointer MkFloat_entry();
pointer MkInt_entry();
pointer MkIORes$_entry();
pointer MkIORes_entry();
pointer MkChar_entry();
pointer Empty_entry();
pointer Just_entry();
pointer Nothing_entry();
pointer False_entry();
pointer True_entry();
pointer Nil_entry();
pointer Cons_entry();
pointer main_math_t1_t1_entry();
pointer main_math_t1_entry();
pointer main_math_t2_t1_entry();
pointer main_math_t2_entry();
pointer main_math_entry();
pointer main_t1_t1_entry();
pointer main_t1_t2_entry();
pointer main_t1_entry();
pointer main_t2_t1_entry();
pointer main_t2_t2_entry();
pointer main_t2_entry();
pointer main_entry();
pointer alt1();
pointer if_entry();
pointer id_entry();
pointer map_t1_entry();
pointer map_t2_entry();
pointer alt2();
pointer map_entry();
pointer compose_t1_entry();
pointer compose_entry();
pointer const_entry();
pointer foldr_foldrHelp_t2_entry();
pointer foldr_foldrHelp_entry();
pointer foldr_entry();
pointer alt3();
pointer caseList_entry();
pointer isNil_constFalse_entry();
pointer isNil_t2_entry();
pointer isNil_entry();
pointer alt6();
pointer alt5();
pointer alt4();
pointer primArith_entry();
pointer plus_t1_t0_entry();
pointer plus_t1_entry();
pointer plus_entry();
pointer minus_t1_t0_entry();
pointer minus_t1_entry();
pointer minus_entry();
pointer multiply_t1_t0_entry();
pointer multiply_t1_entry();
pointer multiply_entry();
pointer divide_t1_t0_entry();
pointer divide_t1_entry();
pointer divide_entry();
pointer done_t1_entry();
pointer done_entry();
pointer seq_t2_entry();
pointer seq_entry();
pointer return_entry();
pointer alt7();
pointer bind_entry();
pointer sequence_entry();
pointer getc_t1_entry();
pointer getc_entry();
pointer putc_t1_entry();
pointer alt8();
pointer putc_entry();
pointer puts_t1_entry();
pointer puts_entry();
pointer puts_info[] = {(pointer)puts_entry};
pointer puts_t1_info[] = {(pointer)puts_t1_entry};
pointer putc_info[] = {(pointer)putc_entry};
pointer putc_t1_info[] = {(pointer)putc_t1_entry};
pointer getc_info[] = {(pointer)getc_entry};
pointer getc_t1_info[] = {(pointer)getc_t1_entry};
pointer sequence_info[] = {(pointer)sequence_entry};
pointer bind_info[] = {(pointer)bind_entry};
pointer return_info[] = {(pointer)return_entry};
pointer seq_info[] = {(pointer)seq_entry};
pointer seq_t2_info[] = {(pointer)seq_t2_entry};
pointer done_info[] = {(pointer)done_entry};
pointer done_t1_info[] = {(pointer)done_t1_entry};
pointer divide_info[] = {(pointer)divide_entry};
pointer divide_t1_info[] = {(pointer)divide_t1_entry};
pointer divide_t1_t0_info[] = {(pointer)divide_t1_t0_entry};
pointer multiply_info[] = {(pointer)multiply_entry};
pointer multiply_t1_info[] = {(pointer)multiply_t1_entry};
pointer multiply_t1_t0_info[] = {(pointer)multiply_t1_t0_entry};
pointer minus_info[] = {(pointer)minus_entry};
pointer minus_t1_info[] = {(pointer)minus_t1_entry};
pointer minus_t1_t0_info[] = {(pointer)minus_t1_t0_entry};
pointer plus_info[] = {(pointer)plus_entry};
pointer plus_t1_info[] = {(pointer)plus_t1_entry};
pointer plus_t1_t0_info[] = {(pointer)plus_t1_t0_entry};
pointer primArith_info[] = {(pointer)primArith_entry};
pointer isNil_info[] = {(pointer)isNil_entry};
pointer isNil_t2_info[] = {(pointer)isNil_t2_entry};
pointer isNil_constFalse_info[] = {(pointer)isNil_constFalse_entry};
pointer caseList_info[] = {(pointer)caseList_entry};
pointer foldr_info[] = {(pointer)foldr_entry};
pointer foldr_foldrHelp_info[] = {(pointer)foldr_foldrHelp_entry};
pointer foldr_foldrHelp_t2_info[] = {(pointer)foldr_foldrHelp_t2_entry};
pointer const_info[] = {(pointer)const_entry};
pointer compose_info[] = {(pointer)compose_entry};
pointer compose_t1_info[] = {(pointer)compose_t1_entry};
pointer map_info[] = {(pointer)map_entry};
pointer map_t2_info[] = {(pointer)map_t2_entry};
pointer map_t1_info[] = {(pointer)map_t1_entry};
pointer id_info[] = {(pointer)id_entry};
pointer if_info[] = {(pointer)if_entry};
pointer main_info[] = {(pointer)main_entry};
pointer main_t2_info[] = {(pointer)main_t2_entry};
pointer main_t2_t2_info[] = {(pointer)main_t2_t2_entry};
pointer main_t2_t1_info[] = {(pointer)main_t2_t1_entry};
pointer main_t1_info[] = {(pointer)main_t1_entry};
pointer main_t1_t2_info[] = {(pointer)main_t1_t2_entry};
pointer main_t1_t1_info[] = {(pointer)main_t1_t1_entry};
pointer main_math_info[] = {(pointer)main_math_entry};
pointer main_math_t2_info[] = {(pointer)main_math_t2_entry};
pointer main_math_t2_t1_info[] = {(pointer)main_math_t2_t1_entry};
pointer main_math_t1_info[] = {(pointer)main_math_t1_entry};
pointer main_math_t1_t1_info[] = {(pointer)main_math_t1_t1_entry};
pointer MkFloat_info[] = {(pointer)MkFloat_entry};
pointer MkInt_info[] = {(pointer)MkInt_entry};
pointer MkIORes$_info[] = {(pointer)MkIORes$_entry};
pointer MkIORes_info[] = {(pointer)MkIORes_entry};
pointer MkChar_info[] = {(pointer)MkChar_entry};
pointer Empty_info[] = {(pointer)Empty_entry};
pointer Just_info[] = {(pointer)Just_entry};
pointer Nothing_info[] = {(pointer)Nothing_entry};
pointer False_info[] = {(pointer)False_entry};
pointer True_info[] = {(pointer)True_entry};
pointer Nil_info[] = {(pointer)Nil_entry};
pointer Cons_info[] = {(pointer)Cons_entry};
pointer puts_closure[] = {(pointer)puts_info};
pointer putc_closure[] = {(pointer)putc_info};
pointer getc_closure[] = {(pointer)getc_info};
pointer sequence_closure[] = {(pointer)sequence_info};
pointer bind_closure[] = {(pointer)bind_info};
pointer return_closure[] = {(pointer)return_info};
pointer seq_closure[] = {(pointer)seq_info};
pointer done_closure[] = {(pointer)done_info};
pointer divide_closure[] = {(pointer)divide_info};
pointer multiply_closure[] = {(pointer)multiply_info};
pointer minus_closure[] = {(pointer)minus_info};
pointer plus_closure[] = {(pointer)plus_info};
pointer primArith_closure[] = {(pointer)primArith_info};
pointer isNil_closure[] = {(pointer)isNil_info};
pointer caseList_closure[] = {(pointer)caseList_info};
pointer foldr_closure[] = {(pointer)foldr_info};
pointer const_closure[] = {(pointer)const_info};
pointer compose_closure[] = {(pointer)compose_info};
pointer map_closure[] = {(pointer)map_info};
pointer id_closure[] = {(pointer)id_info};
pointer if_closure[] = {(pointer)if_info};
pointer main_closure[] = {(pointer)main_info};

pointer MkFloat_entry() {
    printf("MkFloat\n");
    RTag = 12;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkInt_entry() {
    printf("MkInt %ld\n", (intptr_t)Node[1]);
    RTag = 11;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkIORes$_entry() {
    printf("MkIORes$\n");
    RTag = 10;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkIORes_entry() {
    printf("MkIORes\n");
    RTag = 9;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkChar_entry() {
    printf("MkChar\n");
    RTag = 8;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Empty_entry() {
    printf("Empty\n");
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

pointer main_math_t1_t1_entry() {
    printf("main_math_t1_t1\n");
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

pointer main_math_t1_entry() {
    printf("main_math_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_math_t1_t1 */
    Hp[0] = (pointer)(main_math_t1_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push main_math_t1_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(plus_closure); /* Grab plus into Node */
    ENTER((pointer**)Node);        /* Enter plus */
}

pointer main_math_t2_t1_entry() {
    printf("main_math_t2_t1\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)(MkInt_info);
    Hp[1] = (pointer)(5);          /* 5 */
    Node = (pointer*)(Hp);         /* Grab MkInt into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
}

pointer main_math_t2_entry() {
    printf("main_math_t2\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_math_t2_t1 */
    Hp[0] = (pointer)(main_math_t2_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push main_math_t2_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(multiply_closure); /* Grab multiply into Node */
    ENTER((pointer**)Node);        /* Enter multiply */
}

pointer main_math_entry() {
    printf("main_math\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_math_t1 */
    Hp[0] = (pointer)(main_math_t1_info);
    /* Fill in closure for main_math_t2 */
    Hp[1] = (pointer)(main_math_t2_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp + 1);   /* Push main_math_t2 onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push main_math_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(compose_closure); /* Grab compose into Node */
    ENTER((pointer**)Node);        /* Enter compose */
}

pointer main_t1_t1_entry() {
    printf("main_t1_t1\n");
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

pointer main_t1_t2_entry() {
    printf("main_t1_t2\n");
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

pointer main_t1_entry() {
    printf("main_t1\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_t1_t1 */
    Hp[0] = (pointer)(main_t1_t1_info);
    /* Fill in closure for main_t1_t2 */
    Hp[1] = (pointer)(main_t1_t2_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp + 1);   /* Push main_t1_t2 onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push main_t1_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(multiply_closure); /* Grab multiply into Node */
    ENTER((pointer**)Node);        /* Enter multiply */
}

pointer main_t2_t1_entry() {
    printf("main_t2_t1\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)(MkInt_info);
    Hp[1] = (pointer)(100);        /* 100 */
    Node = (pointer*)(Hp);         /* Grab MkInt into Node */
    ENTER((pointer**)Node);        /* Enter MkInt */
}

pointer main_t2_t2_entry() {
    printf("main_t2_t2\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkInt */
    Hp[0] = (pointer)(MkInt_info);
    Hp[1] = (pointer)(50);         /* 50 */
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
    /* Fill in closure for main_t2_t1 */
    Hp[0] = (pointer)(main_t2_t1_info);
    /* Fill in closure for main_t2_t2 */
    Hp[1] = (pointer)(main_t2_t2_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp + 1);   /* Push main_t2_t2 onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push main_t2_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(divide_closure); /* Grab divide into Node */
    ENTER((pointer**)Node);        /* Enter divide */
}

pointer main_entry() {
    printf("main\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for main_math */
    Hp[0] = (pointer)(main_math_info);
    /* Evaluate body */
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
    Node = (pointer*)(minus_closure); /* Grab minus into Node */
    ENTER((pointer**)Node);        /* Enter minus */
}

pointer alt1() {
    printf("alt1\n");
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
    SpB[1] = (pointer)(alt1);
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
    SpA[-1] = (pointer)(Node[2]);  /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node[1]);    /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map_t2_entry() {
    printf("map_t2\n");
    SpA[-1] = (pointer)(Node[2]);  /* Push ys onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(map_closure); /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer alt2() {
    printf("alt2\n");
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
            Hp[2] = (pointer)(Node[1]);    /* y */
            /* Fill in closure for map_t2 */
            Hp[3] = (pointer)(map_t2_info);
            Hp[4] = (pointer)(SpA[0]);     /* f */
            Hp[5] = (pointer)(Node[2]);    /* ys */
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
    SpB[1] = (pointer)(alt2);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    pointer a1 = SpA[1];           /* Grab xs into a local variable */
    Node = (pointer*)(a1);         /* Grab xs into Node */
    ENTER((pointer**)Node);        /* Enter xs */
}

pointer compose_t1_entry() {
    printf("compose_t1\n");
    SpA[-1] = (pointer)(Node[2]);  /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node[1]);    /* Grab g into Node */
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

pointer const_entry() {
    printf("const\n");
    pointer a0 = SpA[0];           /* Grab a into a local variable */
    pointer a1 = SpA[1];           /* Grab b into a local variable */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab a into Node */
    ENTER((pointer**)Node);        /* Enter a */
}

pointer foldr_foldrHelp_t2_entry() {
    printf("foldr_foldrHelp_t2\n");
    SpA[-1] = (pointer)(Node[3]);  /* Push xs onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push b onto stack */
    SpA[-3] = (pointer)(Node[2]);  /* Push f onto stack */
    SpA = SpA - 3;                 /* Adjust SpA */
    Node = (pointer*)(foldr_closure); /* Grab foldr into Node */
    ENTER((pointer**)Node);        /* Enter foldr */
}

pointer foldr_foldrHelp_entry() {
    printf("foldr_foldrHelp\n");
    Hp = Hp - 4;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for foldr_foldrHelp_t2 */
    Hp[0] = (pointer)(foldr_foldrHelp_t2_info);
    Hp[1] = (pointer)(Node[1]);    /* b */
    Hp[2] = (pointer)(Node[2]);    /* f */
    Hp[3] = (pointer)(SpA[1]);     /* xs */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab x into a local variable */
    pointer a1 = SpA[1];           /* Grab xs into a local variable */
    SpA[1] = (pointer)(Hp);        /* Push foldr_foldrHelp_t2 onto stack */
    SpA[0] = (pointer)(a0);        /* Push x onto stack */
    Node = (pointer*)(Node[2]);    /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer foldr_entry() {
    printf("foldr\n");
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for foldr_foldrHelp */
    Hp[0] = (pointer)(foldr_foldrHelp_info);
    Hp[1] = (pointer)(SpA[1]);     /* b */
    Hp[2] = (pointer)(SpA[0]);     /* f */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab f into a local variable */
    pointer a1 = SpA[1];           /* Grab b into a local variable */
    pointer a2 = SpA[2];           /* Grab as into a local variable */
    SpA[2] = (pointer)(Hp);        /* Push foldr_foldrHelp onto stack */
    SpA[1] = (pointer)(a1);        /* Push b onto stack */
    SpA[0] = (pointer)(a2);        /* Push as onto stack */
    Node = (pointer*)(caseList_closure); /* Grab caseList into Node */
    ENTER((pointer**)Node);        /* Enter caseList */
}

pointer alt3() {
    printf("alt3\n");
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
            SpA[2] = (pointer)(Node[2]);   /* Push tail onto stack */
            SpA[1] = (pointer)(Node[1]);   /* Push head onto stack */
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
    SpB[1] = (pointer)(alt3);
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

pointer alt6() {
    printf("alt6\n");
    switch (IntReg) {
        default:
        {
            intptr_t t$ = IntReg;
            Hp = Hp - 2;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for MkInt */
            Hp[0] = (pointer)(MkInt_info);
            Hp[1] = (pointer)(t$);         /* t$ */
            SpA = SpA + 3;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab MkInt into Node */
            ENTER((pointer**)Node);        /* Enter MkInt */
            break;
        }
    }
    JUMP(main);
}

pointer alt5() {
    printf("alt5\n");
    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save y$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt6);
            SpB = SpB + 1;
            /* Evaluate body */
            pointer a0 = SpA[0];           /* Grab op into a local variable */
            pointer a1 = SpA[1];           /* Grab e1 into a local variable */
            pointer a2 = SpA[2];           /* Grab e2 into a local variable */
            pointer b2 = SpB[-2];          /* Grab x$ into a local variable */
            pointer b1 = SpB[-1];          /* Grab y$ into a local variable */
            SpB[1] = (pointer)(b1);        /* Push y$ onto stack */
            SpB[2] = (pointer)(b2);        /* Push x$ onto stack */
            SpB = SpB + 2;                 /* Adjust SpB */
            Node = (pointer*)(a0);         /* Grab op into Node */
            ENTER((pointer**)Node);        /* Enter op */
            break;
        }
    }
    JUMP(main);
}

pointer alt4() {
    printf("alt4\n");
    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save x$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt5);
            SpB = SpB + 1;
            /* Evaluate body */
            pointer a0 = SpA[0];           /* Grab op into a local variable */
            pointer a1 = SpA[1];           /* Grab e1 into a local variable */
            pointer a2 = SpA[2];           /* Grab e2 into a local variable */
            pointer b1 = SpB[-1];          /* Grab x$ into a local variable */
            Node = (pointer*)(a2);         /* Grab e2 into Node */
            ENTER((pointer**)Node);        /* Enter e2 */
            break;
        }
    }
    JUMP(main);
}

pointer primArith_entry() {
    printf("primArith\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt4);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab op into a local variable */
    pointer a1 = SpA[1];           /* Grab e1 into a local variable */
    pointer a2 = SpA[2];           /* Grab e2 into a local variable */
    Node = (pointer*)(a1);         /* Grab e1 into Node */
    ENTER((pointer**)Node);        /* Enter e1 */
}

pointer plus_t1_t0_entry() {
    printf("plus_t1_t0\n");
    IntReg = (intptr_t)SpB[0] + (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer plus_t1_entry() {
    printf("plus_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for plus_t1_t0 */
    Hp[0] = (pointer)(plus_t1_t0_info);
    /* Evaluate body */
    Node = (pointer*)(Hp);         /* Grab plus_t1_t0 into Node */
    ENTER((pointer**)Node);        /* Enter plus_t1_t0 */
}

pointer plus_entry() {
    printf("plus\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for plus_t1 */
    Hp[0] = (pointer)(plus_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push plus_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(primArith_closure); /* Grab primArith into Node */
    ENTER((pointer**)Node);        /* Enter primArith */
}

pointer minus_t1_t0_entry() {
    printf("minus_t1_t0\n");
    IntReg = (intptr_t)SpB[0] - (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer minus_t1_entry() {
    printf("minus_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for minus_t1_t0 */
    Hp[0] = (pointer)(minus_t1_t0_info);
    /* Evaluate body */
    Node = (pointer*)(Hp);         /* Grab minus_t1_t0 into Node */
    ENTER((pointer**)Node);        /* Enter minus_t1_t0 */
}

pointer minus_entry() {
    printf("minus\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for minus_t1 */
    Hp[0] = (pointer)(minus_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push minus_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(primArith_closure); /* Grab primArith into Node */
    ENTER((pointer**)Node);        /* Enter primArith */
}

pointer multiply_t1_t0_entry() {
    printf("multiply_t1_t0\n");
    IntReg = (intptr_t)SpB[0] * (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer multiply_t1_entry() {
    printf("multiply_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for multiply_t1_t0 */
    Hp[0] = (pointer)(multiply_t1_t0_info);
    /* Evaluate body */
    Node = (pointer*)(Hp);         /* Grab multiply_t1_t0 into Node */
    ENTER((pointer**)Node);        /* Enter multiply_t1_t0 */
}

pointer multiply_entry() {
    printf("multiply\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for multiply_t1 */
    Hp[0] = (pointer)(multiply_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push multiply_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(primArith_closure); /* Grab primArith into Node */
    ENTER((pointer**)Node);        /* Enter primArith */
}

pointer divide_t1_t0_entry() {
    printf("divide_t1_t0\n");
    IntReg = (intptr_t)SpB[0] / (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer divide_t1_entry() {
    printf("divide_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for divide_t1_t0 */
    Hp[0] = (pointer)(divide_t1_t0_info);
    /* Evaluate body */
    Node = (pointer*)(Hp);         /* Grab divide_t1_t0 into Node */
    ENTER((pointer**)Node);        /* Enter divide_t1_t0 */
}

pointer divide_entry() {
    printf("divide\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for divide_t1 */
    Hp[0] = (pointer)(divide_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push divide_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(primArith_closure); /* Grab primArith into Node */
    ENTER((pointer**)Node);        /* Enter primArith */
}

pointer done_t1_entry() {
    printf("done_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for Empty */
    Hp[0] = (pointer)(Empty_info);
    Node = (pointer*)(Hp);         /* Grab Empty into Node */
    ENTER((pointer**)Node);        /* Enter Empty */
}

pointer done_entry() {
    printf("done\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for done_t1 */
    Hp[0] = (pointer)(done_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push done_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(return_closure); /* Grab return into Node */
    ENTER((pointer**)Node);        /* Enter return */
}

pointer seq_t2_entry() {
    printf("seq_t2\n");
    SpA[-1] = (pointer)(Node[1]);  /* Push n onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(const_closure); /* Grab const into Node */
    ENTER((pointer**)Node);        /* Enter const */
}

pointer seq_entry() {
    printf("seq\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for seq_t2 */
    Hp[0] = (pointer)(seq_t2_info);
    Hp[1] = (pointer)(SpA[1]);     /* n */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab m into a local variable */
    pointer a1 = SpA[1];           /* Grab n into a local variable */
    SpA[1] = (pointer)(Hp);        /* Push seq_t2 onto stack */
    SpA[0] = (pointer)(a0);        /* Push m onto stack */
    Node = (pointer*)(bind_closure); /* Grab bind into Node */
    ENTER((pointer**)Node);        /* Enter bind */
}

pointer return_entry() {
    printf("return\n");
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkIORes */
    Hp[0] = (pointer)(MkIORes_info);
    Hp[1] = (pointer)(SpA[0]);     /* a */
    Hp[2] = (pointer)(SpA[1]);     /* w */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab MkIORes into Node */
    ENTER((pointer**)Node);        /* Enter MkIORes */
}

pointer alt7() {
    printf("alt7\n");
    switch (RTag) {
        case 9:
        {
            pointer a0 = SpA[0];           /* Grab m into a local variable */
            pointer a1 = SpA[1];           /* Grab k into a local variable */
            pointer a2 = SpA[2];           /* Grab w into a local variable */
            SpA[2] = (pointer)(Node[2]);   /* Push ww onto stack */
            SpA[1] = (pointer)(Node[1]);   /* Push a onto stack */
            SpA = SpA + 1;                 /* Adjust SpA */
            Node = (pointer*)(a1);         /* Grab k into Node */
            ENTER((pointer**)Node);        /* Enter k */
            break;
        }
    }
    JUMP(main);
}

pointer bind_entry() {
    printf("bind\n");
    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt7);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab m into a local variable */
    pointer a1 = SpA[1];           /* Grab k into a local variable */
    pointer a2 = SpA[2];           /* Grab w into a local variable */
    SpA[-1] = (pointer)(a2);       /* Push w onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab m into Node */
    ENTER((pointer**)Node);        /* Enter m */
}

pointer sequence_entry() {
    printf("sequence\n");
    SpA[-1] = (pointer)(done_closure); /* Push done onto stack */
    SpA[-2] = (pointer)(seq_closure); /* Push seq onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(foldr_closure); /* Grab foldr into Node */
    ENTER((pointer**)Node);        /* Enter foldr */
}

pointer getc_t1_entry() {
    printf("getc_t1\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkChar */
    Hp[0] = (pointer)(MkChar_info);
    Hp[1] = (pointer)(Node[1]);    /* n$ */
    Node = (pointer*)(Hp);         /* Grab MkChar into Node */
    ENTER((pointer**)Node);        /* Enter MkChar */
}

pointer getc_entry() {
    printf("getc\n");
    intptr_t n$ = getchar();
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getc_t1 */
    Hp[0] = (pointer)(getc_t1_info);
    Hp[1] = (pointer)(n$);         /* n$ */
    /* Evaluate body */
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkIORes */
    Hp[0] = (pointer)(MkIORes_info);
    Hp[1] = (pointer)(Hp + 3);     /* getc_t1 */
    Hp[2] = (pointer)(ww_closure); /* ww */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab MkIORes into Node */
    ENTER((pointer**)Node);        /* Enter MkIORes */
}

pointer putc_t1_entry() {
    printf("putc_t1\n");
    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for Empty */
    Hp[0] = (pointer)(Empty_info);
    Node = (pointer*)(Hp);         /* Grab Empty into Node */
    ENTER((pointer**)Node);        /* Enter Empty */
}

pointer alt8() {
    printf("alt8\n");
    switch (RTag) {
        case 8:
        {
            intptr_t n$ = putchar(Node[1]);
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for putc_t1 */
            Hp[0] = (pointer)(putc_t1_info);
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for MkIORes */
            Hp[0] = (pointer)(MkIORes_info);
            Hp[1] = (pointer)(Hp + 3);     /* putc_t1 */
            Hp[2] = (pointer)(ww_closure); /* ww */
            SpA = SpA + 3;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab MkIORes into Node */
            ENTER((pointer**)Node);        /* Enter MkIORes */
            break;
        }
    }
    JUMP(main);
}

pointer putc_entry() {
    printf("putc\n");
    /* Save local environment */
    SpA[-1] = (pointer)(Node[1]);  /* Save putchar */
    SpA = SpA - 1;                 /* Adjust SpA */
    /* Push return address */
    SpB[1] = (pointer)(alt8);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a1 = SpA[1];           /* Grab c into a local variable */
    pointer a2 = SpA[2];           /* Grab w into a local variable */
    pointer a0 = SpA[0];           /* Grab putchar into a local variable */
    Node = (pointer*)(a1);         /* Grab c into Node */
    ENTER((pointer**)Node);        /* Enter c */
}

pointer puts_t1_entry() {
    printf("puts_t1\n");
    SpA[-1] = (pointer)(Node[1]);  /* Push as onto stack */
    SpA[-2] = (pointer)(putc_closure); /* Push putc onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(map_closure); /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer puts_entry() {
    printf("puts\n");
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for puts_t1 */
    Hp[0] = (pointer)(puts_t1_info);
    Hp[1] = (pointer)(SpA[0]);     /* as */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab as into a local variable */
    SpA[0] = (pointer)(Hp);        /* Push puts_t1 onto stack */
    Node = (pointer*)(sequence_closure); /* Grab sequence into Node */
    ENTER((pointer**)Node);        /* Enter sequence */
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
