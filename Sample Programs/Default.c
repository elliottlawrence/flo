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
pointer* Hp = Heap + 10000;
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
pointer main_entry();
pointer echo_echo1_t1_t2_entry();
pointer echo_echo1_t1_entry();
pointer echo_echo1_t3_t1_entry();
pointer echo_echo1_t3_entry();
pointer echo_echo1_entry();
pointer echo_entry();
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
pointer eqChar_entry();
pointer newLine_entry();
pointer alt9();
pointer alt8();
pointer alt7();
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
pointer alt12();
pointer alt11();
pointer alt10();
pointer eqInt_entry();
pointer done_t1_entry();
pointer done_entry();
pointer seq_t2_entry();
pointer seq_entry();
pointer return_entry();
pointer alt13();
pointer bind_entry();
pointer sequence_entry();
pointer getChar_t1_entry();
pointer getChar_entry();
pointer putChar_t1_entry();
pointer alt14();
pointer putChar_entry();
pointer print_t1_entry();
pointer print_entry();
pointer printLn_t1_entry();
pointer printLn_t2_entry();
pointer printLn_entry();
pointer getLine_getLine1_getLine2_t1_entry();
pointer getLine_getLine1_getLine2_entry();
pointer getLine_getLine1_t1_entry();
pointer getLine_getLine1_t2_t1_entry();
pointer getLine_getLine1_t2_entry();
pointer getLine_getLine1_t3_entry();
pointer getLine_getLine1_entry();
pointer getLine_entry();
pointer forever_a1_entry();
pointer forever_entry();
pointer forever_info[] = {(pointer)forever_entry};
pointer forever_a1_info[] = {(pointer)forever_a1_entry};
pointer getLine_info[] = {(pointer)getLine_entry};
pointer getLine_getLine1_info[] = {(pointer)getLine_getLine1_entry};
pointer getLine_getLine1_t3_info[] = {(pointer)getLine_getLine1_t3_entry};
pointer getLine_getLine1_t2_info[] = {(pointer)getLine_getLine1_t2_entry};
pointer getLine_getLine1_t2_t1_info[] = {(pointer)getLine_getLine1_t2_t1_entry};
pointer getLine_getLine1_t1_info[] = {(pointer)getLine_getLine1_t1_entry};
pointer getLine_getLine1_getLine2_info[] = {(pointer)getLine_getLine1_getLine2_entry};
pointer getLine_getLine1_getLine2_t1_info[] = {(pointer)getLine_getLine1_getLine2_t1_entry};
pointer printLn_info[] = {(pointer)printLn_entry};
pointer printLn_t2_info[] = {(pointer)printLn_t2_entry};
pointer printLn_t1_info[] = {(pointer)printLn_t1_entry};
pointer print_info[] = {(pointer)print_entry};
pointer print_t1_info[] = {(pointer)print_t1_entry};
pointer putChar_info[] = {(pointer)putChar_entry};
pointer putChar_t1_info[] = {(pointer)putChar_t1_entry};
pointer getChar_info[] = {(pointer)getChar_entry};
pointer getChar_t1_info[] = {(pointer)getChar_t1_entry};
pointer sequence_info[] = {(pointer)sequence_entry};
pointer bind_info[] = {(pointer)bind_entry};
pointer return_info[] = {(pointer)return_entry};
pointer seq_info[] = {(pointer)seq_entry};
pointer seq_t2_info[] = {(pointer)seq_t2_entry};
pointer done_info[] = {(pointer)done_entry};
pointer done_t1_info[] = {(pointer)done_t1_entry};
pointer eqInt_info[] = {(pointer)eqInt_entry};
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
pointer newLine_info[] = {(pointer)newLine_entry};
pointer eqChar_info[] = {(pointer)eqChar_entry};
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
pointer echo_info[] = {(pointer)echo_entry};
pointer echo_echo1_info[] = {(pointer)echo_echo1_entry};
pointer echo_echo1_t3_info[] = {(pointer)echo_echo1_t3_entry};
pointer echo_echo1_t3_t1_info[] = {(pointer)echo_echo1_t3_t1_entry};
pointer echo_echo1_t1_info[] = {(pointer)echo_echo1_t1_entry};
pointer echo_echo1_t1_t2_info[] = {(pointer)echo_echo1_t1_t2_entry};
pointer main_info[] = {(pointer)main_entry};
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
pointer forever_closure[] = {(pointer)forever_info};
pointer getLine_closure[] = {(pointer)getLine_info};
pointer printLn_closure[] = {(pointer)printLn_info};
pointer print_closure[] = {(pointer)print_info};
pointer putChar_closure[] = {(pointer)putChar_info};
pointer getChar_closure[] = {(pointer)getChar_info};
pointer sequence_closure[] = {(pointer)sequence_info};
pointer bind_closure[] = {(pointer)bind_info};
pointer return_closure[] = {(pointer)return_info};
pointer seq_closure[] = {(pointer)seq_info};
pointer done_closure[] = {(pointer)done_info};
pointer eqInt_closure[] = {(pointer)eqInt_info};
pointer divide_closure[] = {(pointer)divide_info};
pointer multiply_closure[] = {(pointer)multiply_info};
pointer minus_closure[] = {(pointer)minus_info};
pointer plus_closure[] = {(pointer)plus_info};
pointer primArith_closure[] = {(pointer)primArith_info};
pointer newLine_closure[] = {(pointer)newLine_info};
pointer eqChar_closure[] = {(pointer)eqChar_info};
pointer isNil_closure[] = {(pointer)isNil_info};
pointer caseList_closure[] = {(pointer)caseList_info};
pointer foldr_closure[] = {(pointer)foldr_info};
pointer const_closure[] = {(pointer)const_info};
pointer compose_closure[] = {(pointer)compose_info};
pointer map_closure[] = {(pointer)map_info};
pointer id_closure[] = {(pointer)id_info};
pointer if_closure[] = {(pointer)if_info};
pointer echo_closure[] = {(pointer)echo_info};
pointer main_closure[] = {(pointer)main_info};

pointer MkFloat_entry() {

    RTag = 12;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkInt_entry() {

    RTag = 11;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkIORes$_entry() {

    RTag = 10;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkIORes_entry() {

    RTag = 9;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer MkChar_entry() {

    RTag = 8;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Empty_entry() {

    RTag = 7;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Just_entry() {

    RTag = 6;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Nothing_entry() {

    RTag = 5;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer False_entry() {

    RTag = 4;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer True_entry() {

    RTag = 3;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Nil_entry() {

    RTag = 2;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer Cons_entry() {

    RTag = 1;
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer main_entry() {

    Node = (pointer*)(echo_closure); /* Grab echo into Node */
    ENTER((pointer**)Node);        /* Enter echo */
}

pointer echo_echo1_t1_t2_entry() {

    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkChar */
    Hp[0] = (pointer)(MkChar_info);
    Hp[1] = (pointer)(-1);         /* -1 */
    Node = (pointer*)(Hp);         /* Grab MkChar into Node */
    ENTER((pointer**)Node);        /* Enter MkChar */
}

pointer echo_echo1_t1_entry() {

    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for echo_echo1_t1_t2 */
    Hp[0] = (pointer)(echo_echo1_t1_t2_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push echo_echo1_t1_t2 onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push a onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(eqChar_closure); /* Grab eqChar into Node */
    ENTER((pointer**)Node);        /* Enter eqChar */
}

pointer echo_echo1_t3_t1_entry() {

    SpA[-1] = (pointer)(Node[1]);  /* Push a onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(putChar_closure); /* Grab putChar into Node */
    ENTER((pointer**)Node);        /* Enter putChar */
}

pointer echo_echo1_t3_entry() {

    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for echo_echo1_t3_t1 */
    Hp[0] = (pointer)(echo_echo1_t3_t1_info);
    Hp[1] = (pointer)(Node[1]);    /* a */
    /* Evaluate body */
    SpA[-1] = (pointer)(echo_closure); /* Push echo onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push echo_echo1_t3_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(seq_closure); /* Grab seq into Node */
    ENTER((pointer**)Node);        /* Enter seq */
}

pointer echo_echo1_entry() {

    Hp = Hp - 4;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for echo_echo1_t1 */
    Hp[0] = (pointer)(echo_echo1_t1_info);
    Hp[1] = (pointer)(SpA[0]);     /* a */
    /* Fill in closure for echo_echo1_t3 */
    Hp[2] = (pointer)(echo_echo1_t3_info);
    Hp[3] = (pointer)(SpA[0]);     /* a */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab a into a local variable */
    SpA[0] = (pointer)(Hp + 2);    /* Push echo_echo1_t3 onto stack */
    SpA[-1] = (pointer)(done_closure); /* Push done onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push echo_echo1_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(if_closure); /* Grab if into Node */
    ENTER((pointer**)Node);        /* Enter if */
}

pointer echo_entry() {

    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for echo_echo1 */
    Hp[0] = (pointer)(echo_echo1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push echo_echo1 onto stack */
    SpA[-2] = (pointer)(getChar_closure); /* Push getChar onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(bind_closure); /* Grab bind into Node */
    ENTER((pointer**)Node);        /* Enter bind */
}

pointer alt1() {

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

    pointer a0 = SpA[0];           /* Grab x into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab x into Node */
    ENTER((pointer**)Node);        /* Enter x */
}

pointer map_t1_entry() {

    SpA[-1] = (pointer)(Node[2]);  /* Push y onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node[1]);    /* Grab f into Node */
    ENTER((pointer**)Node);        /* Enter f */
}

pointer map_t2_entry() {

    SpA[-1] = (pointer)(Node[2]);  /* Push ys onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push f onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(map_closure); /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer alt2() {

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

    SpA[-1] = (pointer)(Node[2]);  /* Push x onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(Node[1]);    /* Grab g into Node */
    ENTER((pointer**)Node);        /* Enter g */
}

pointer compose_entry() {

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

    pointer a0 = SpA[0];           /* Grab a into a local variable */
    pointer a1 = SpA[1];           /* Grab b into a local variable */
    SpA = SpA + 2;                 /* Adjust SpA */
    Node = (pointer*)(a0);         /* Grab a into Node */
    ENTER((pointer**)Node);        /* Enter a */
}

pointer foldr_foldrHelp_t2_entry() {

    SpA[-1] = (pointer)(Node[3]);  /* Push xs onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push b onto stack */
    SpA[-3] = (pointer)(Node[2]);  /* Push f onto stack */
    SpA = SpA - 3;                 /* Adjust SpA */
    Node = (pointer*)(foldr_closure); /* Grab foldr into Node */
    ENTER((pointer**)Node);        /* Enter foldr */
}

pointer foldr_foldrHelp_entry() {

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

    switch (IntReg) {
        case 0:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for False */
            Hp[0] = (pointer)(False_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab False into Node */
            ENTER((pointer**)Node);        /* Enter False */
            break;
        }
        case 1:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for True */
            Hp[0] = (pointer)(True_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab True into Node */
            ENTER((pointer**)Node);        /* Enter True */
            break;
        }
    }
    JUMP(main);
}

pointer alt5() {

    switch (RTag) {
        case 8:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save b$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt6);
            SpB = SpB + 1;
            /* Evaluate body */
            IntReg = (intptr_t)SpB[-2] == (intptr_t)SpB[-1];
            SpB = SpB - 1;
            JUMP(SpB[1]);                  /* Enter return address */
            break;
        }
    }
    JUMP(main);
}

pointer alt4() {

    switch (RTag) {
        case 8:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save a$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt5);
            SpB = SpB + 1;
            /* Evaluate body */
            pointer a0 = SpA[0];           /* Grab a into a local variable */
            pointer a1 = SpA[1];           /* Grab b into a local variable */
            pointer b1 = SpB[-1];          /* Grab a$ into a local variable */
            Node = (pointer*)(a1);         /* Grab b into Node */
            ENTER((pointer**)Node);        /* Enter b */
            break;
        }
    }
    JUMP(main);
}

pointer eqChar_entry() {

    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt4);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab a into a local variable */
    pointer a1 = SpA[1];           /* Grab b into a local variable */
    Node = (pointer*)(a0);         /* Grab a into Node */
    ENTER((pointer**)Node);        /* Enter a */
}

pointer newLine_entry() {

    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkChar */
    Hp[0] = (pointer)(MkChar_info);
    Hp[1] = (pointer)(10);         /* 10 */
    Node = (pointer*)(Hp);         /* Grab MkChar into Node */
    ENTER((pointer**)Node);        /* Enter MkChar */
}

pointer alt9() {

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

pointer alt8() {

    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save y$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt9);
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

pointer alt7() {

    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save x$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt8);
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

    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt7);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab op into a local variable */
    pointer a1 = SpA[1];           /* Grab e1 into a local variable */
    pointer a2 = SpA[2];           /* Grab e2 into a local variable */
    Node = (pointer*)(a1);         /* Grab e1 into Node */
    ENTER((pointer**)Node);        /* Enter e1 */
}

pointer plus_t1_t0_entry() {

    IntReg = (intptr_t)SpB[0] + (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer plus_t1_entry() {

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

    IntReg = (intptr_t)SpB[0] - (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer minus_t1_entry() {

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

    IntReg = (intptr_t)SpB[0] * (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer multiply_t1_entry() {

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

    IntReg = (intptr_t)SpB[0] / (intptr_t)SpB[-1];
    SpB = SpB - 2;                 /* Adjust SpB */
    SpB = SpB - 1;
    JUMP(SpB[1]);                  /* Enter return address */
}

pointer divide_t1_entry() {

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

pointer alt12() {

    switch (IntReg) {
        case 0:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for False */
            Hp[0] = (pointer)(False_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab False into Node */
            ENTER((pointer**)Node);        /* Enter False */
            break;
        }
        case 1:
        {
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for True */
            Hp[0] = (pointer)(True_info);
            SpA = SpA + 2;                 /* Adjust SpA */
            SpB = SpB - 2;                 /* Adjust SpB */
            Node = (pointer*)(Hp);         /* Grab True into Node */
            ENTER((pointer**)Node);        /* Enter True */
            break;
        }
    }
    JUMP(main);
}

pointer alt11() {

    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save b$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt12);
            SpB = SpB + 1;
            /* Evaluate body */
            IntReg = (intptr_t)SpB[-2] == (intptr_t)SpB[-1];
            SpB = SpB - 1;
            JUMP(SpB[1]);                  /* Enter return address */
            break;
        }
    }
    JUMP(main);
}

pointer alt10() {

    switch (RTag) {
        case 11:
        {
            /* Save local environment */
            SpB[1] = (pointer)(Node[1]);   /* Save a$ */
            SpB = SpB + 1;                 /* Adjust SpB */
            /* Push return address */
            SpB[1] = (pointer)(alt11);
            SpB = SpB + 1;
            /* Evaluate body */
            pointer a0 = SpA[0];           /* Grab a into a local variable */
            pointer a1 = SpA[1];           /* Grab b into a local variable */
            pointer b1 = SpB[-1];          /* Grab a$ into a local variable */
            Node = (pointer*)(a1);         /* Grab b into Node */
            ENTER((pointer**)Node);        /* Enter b */
            break;
        }
    }
    JUMP(main);
}

pointer eqInt_entry() {

    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt10);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab a into a local variable */
    pointer a1 = SpA[1];           /* Grab b into a local variable */
    Node = (pointer*)(a0);         /* Grab a into Node */
    ENTER((pointer**)Node);        /* Enter a */
}

pointer done_t1_entry() {

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

    SpA[-1] = (pointer)(Node[1]);  /* Push n onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(const_closure); /* Grab const into Node */
    ENTER((pointer**)Node);        /* Enter const */
}

pointer seq_entry() {

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

pointer alt13() {

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

    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt13);
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

    SpA[-1] = (pointer)(done_closure); /* Push done onto stack */
    SpA[-2] = (pointer)(seq_closure); /* Push seq onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(foldr_closure); /* Grab foldr into Node */
    ENTER((pointer**)Node);        /* Enter foldr */
}

pointer getChar_t1_entry() {

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

pointer getChar_entry() {

    intptr_t n$ = getchar();
    intptr_t ww = 0;
    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getChar_t1 */
    Hp[0] = (pointer)(getChar_t1_info);
    Hp[1] = (pointer)(n$);         /* n$ */
    /* Evaluate body */
    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for MkIORes */
    Hp[0] = (pointer)(MkIORes_info);
    Hp[1] = (pointer)(Hp + 3);     /* getChar_t1 */
    Hp[2] = (pointer)(ww);         /* ww */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab MkIORes into Node */
    ENTER((pointer**)Node);        /* Enter MkIORes */
}

pointer putChar_t1_entry() {

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

pointer alt14() {

    switch (RTag) {
        case 8:
        {
            intptr_t n$ = putchar((int)Node[1]);
            intptr_t ww = 0;
            Hp = Hp - 1;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for putChar_t1 */
            Hp[0] = (pointer)(putChar_t1_info);
            /* Evaluate body */
            Hp = Hp - 3;                   /* Allocate some heap */
            if (Hp < HLimit) {
                printf("Error: Out of heap space\n");
                exit(0);
            }
            /* Fill in closure for MkIORes */
            Hp[0] = (pointer)(MkIORes_info);
            Hp[1] = (pointer)(Hp + 3);     /* putChar_t1 */
            Hp[2] = (pointer)(ww);         /* ww */
            SpA = SpA + 2;                 /* Adjust SpA */
            Node = (pointer*)(Hp);         /* Grab MkIORes into Node */
            ENTER((pointer**)Node);        /* Enter MkIORes */
            break;
        }
    }
    JUMP(main);
}

pointer putChar_entry() {

    /* Save local environment */
    /* Push return address */
    SpB[1] = (pointer)(alt14);
    SpB = SpB + 1;
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab c into a local variable */
    pointer a1 = SpA[1];           /* Grab w into a local variable */
    Node = (pointer*)(a0);         /* Grab c into Node */
    ENTER((pointer**)Node);        /* Enter c */
}

pointer print_t1_entry() {

    SpA[-1] = (pointer)(Node[1]);  /* Push string onto stack */
    SpA[-2] = (pointer)(putChar_closure); /* Push putChar onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(map_closure); /* Grab map into Node */
    ENTER((pointer**)Node);        /* Enter map */
}

pointer print_entry() {

    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for print_t1 */
    Hp[0] = (pointer)(print_t1_info);
    Hp[1] = (pointer)(SpA[0]);     /* string */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab string into a local variable */
    SpA[0] = (pointer)(Hp);        /* Push print_t1 onto stack */
    Node = (pointer*)(sequence_closure); /* Grab sequence into Node */
    ENTER((pointer**)Node);        /* Enter sequence */
}

pointer printLn_t1_entry() {

    SpA[-1] = (pointer)(Node[1]);  /* Push string onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(print_closure); /* Grab print into Node */
    ENTER((pointer**)Node);        /* Enter print */
}

pointer printLn_t2_entry() {

    SpA[-1] = (pointer)(newLine_closure); /* Push newLine onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(putChar_closure); /* Grab putChar into Node */
    ENTER((pointer**)Node);        /* Enter putChar */
}

pointer printLn_entry() {

    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for printLn_t1 */
    Hp[0] = (pointer)(printLn_t1_info);
    Hp[1] = (pointer)(SpA[0]);     /* string */
    /* Fill in closure for printLn_t2 */
    Hp[2] = (pointer)(printLn_t2_info);
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab string into a local variable */
    SpA[0] = (pointer)(Hp + 2);    /* Push printLn_t2 onto stack */
    SpA[-1] = (pointer)(Hp);       /* Push printLn_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(seq_closure); /* Grab seq into Node */
    ENTER((pointer**)Node);        /* Enter seq */
}

pointer getLine_getLine1_getLine2_t1_entry() {

    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for Cons */
    Hp[0] = (pointer)(Cons_info);
    Hp[1] = (pointer)(Node[1]);    /* c */
    Hp[2] = (pointer)(Node[2]);    /* line */
    Node = (pointer*)(Hp);         /* Grab Cons into Node */
    ENTER((pointer**)Node);        /* Enter Cons */
}

pointer getLine_getLine1_getLine2_entry() {

    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getLine_getLine1_getLine2_t1 */
    Hp[0] = (pointer)(getLine_getLine1_getLine2_t1_info);
    Hp[1] = (pointer)(Node[1]);    /* c */
    Hp[2] = (pointer)(SpA[0]);     /* line */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab line into a local variable */
    SpA[0] = (pointer)(Hp);        /* Push getLine_getLine1_getLine2_t1 onto stack */
    Node = (pointer*)(return_closure); /* Grab return into Node */
    ENTER((pointer**)Node);        /* Enter return */
}

pointer getLine_getLine1_t1_entry() {

    SpA[-1] = (pointer)(newLine_closure); /* Push newLine onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push c onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(eqChar_closure); /* Grab eqChar into Node */
    ENTER((pointer**)Node);        /* Enter eqChar */
}

pointer getLine_getLine1_t2_t1_entry() {

    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for Nil */
    Hp[0] = (pointer)(Nil_info);
    Node = (pointer*)(Hp);         /* Grab Nil into Node */
    ENTER((pointer**)Node);        /* Enter Nil */
}

pointer getLine_getLine1_t2_entry() {

    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getLine_getLine1_t2_t1 */
    Hp[0] = (pointer)(getLine_getLine1_t2_t1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push getLine_getLine1_t2_t1 onto stack */
    SpA = SpA - 1;                 /* Adjust SpA */
    Node = (pointer*)(return_closure); /* Grab return into Node */
    ENTER((pointer**)Node);        /* Enter return */
}

pointer getLine_getLine1_t3_entry() {

    SpA[-1] = (pointer)(Node[1]);  /* Push getLine_getLine1_getLine2 onto stack */
    SpA[-2] = (pointer)(getLine_closure); /* Push getLine onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(bind_closure); /* Grab bind into Node */
    ENTER((pointer**)Node);        /* Enter bind */
}

pointer getLine_getLine1_entry() {

    Hp = Hp - 2;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getLine_getLine1_getLine2 */
    Hp[0] = (pointer)(getLine_getLine1_getLine2_info);
    Hp[1] = (pointer)(SpA[0]);     /* c */
    /* Evaluate body */
    Hp = Hp - 5;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getLine_getLine1_t1 */
    Hp[0] = (pointer)(getLine_getLine1_t1_info);
    Hp[1] = (pointer)(SpA[0]);     /* c */
    /* Fill in closure for getLine_getLine1_t2 */
    Hp[2] = (pointer)(getLine_getLine1_t2_info);
    /* Fill in closure for getLine_getLine1_t3 */
    Hp[3] = (pointer)(getLine_getLine1_t3_info);
    Hp[4] = (pointer)(Hp + 5);     /* getLine_getLine1_getLine2 */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab c into a local variable */
    SpA[0] = (pointer)(Hp + 3);    /* Push getLine_getLine1_t3 onto stack */
    SpA[-1] = (pointer)(Hp + 2);   /* Push getLine_getLine1_t2 onto stack */
    SpA[-2] = (pointer)(Hp);       /* Push getLine_getLine1_t1 onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(if_closure); /* Grab if into Node */
    ENTER((pointer**)Node);        /* Enter if */
}

pointer getLine_entry() {

    Hp = Hp - 1;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for getLine_getLine1 */
    Hp[0] = (pointer)(getLine_getLine1_info);
    /* Evaluate body */
    SpA[-1] = (pointer)(Hp);       /* Push getLine_getLine1 onto stack */
    SpA[-2] = (pointer)(getChar_closure); /* Push getChar onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(bind_closure); /* Grab bind into Node */
    ENTER((pointer**)Node);        /* Enter bind */
}

pointer forever_a1_entry() {

    SpA[-1] = (pointer)(Node[2]);  /* Push forever_a1 onto stack */
    SpA[-2] = (pointer)(Node[1]);  /* Push a onto stack */
    SpA = SpA - 2;                 /* Adjust SpA */
    Node = (pointer*)(seq_closure); /* Grab seq into Node */
    ENTER((pointer**)Node);        /* Enter seq */
}

pointer forever_entry() {

    Hp = Hp - 3;                   /* Allocate some heap */
    if (Hp < HLimit) {
        printf("Error: Out of heap space\n");
        exit(0);
    }
    /* Fill in closure for forever_a1 */
    Hp[0] = (pointer)(forever_a1_info);
    Hp[1] = (pointer)(SpA[0]);     /* a */
    Hp[2] = (pointer)(Hp);         /* forever_a1 */
    /* Evaluate body */
    pointer a0 = SpA[0];           /* Grab a into a local variable */
    SpA = SpA + 1;                 /* Adjust SpA */
    Node = (pointer*)(Hp);         /* Grab forever_a1 into Node */
    ENTER((pointer**)Node);        /* Enter forever_a1 */
}

int main() {
    function f_main = (function)main;
    function cont = main_entry;
    SpB[0] = (pointer)(f_main);    /* Push return address */
    SpA[0] = (pointer)(0);         /* Push initial world */
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}
