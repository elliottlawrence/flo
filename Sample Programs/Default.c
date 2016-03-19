#include <stdio.h>
#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return((pointer) lbl)
typedef int * pointer;
typedef pointer (* function)();

pointer Stack[10000];
pointer* SpB = Stack;
pointer* SpA = Stack + 9999;
pointer Heap[10000];
pointer* Hp = Heap;
pointer* HLimit = Heap + 9999;
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

pointer main_entry() {
    JUMP(main);
}

pointer caseList_entry() {
    JUMP(main);
}

pointer isNil_entry() {
    JUMP(main);
}

pointer if_entry() {
    JUMP(main);
}

pointer id_entry() {
    Node = SpA[0];
    pointer t0 = SpA[0];
    SpA = SpA + 1;
    ENTER((pointer**)Node);
}

pointer map_entry() {
    JUMP(main);
}

pointer map1_entry() {
    JUMP(main);
}

pointer test_entry() {
    JUMP(main);
}

pointer apply3_entry() {
    Node = SpA[0];
    pointer t0 = SpA[0];
    pointer t1 = SpA[1];
    SpA[1] = t1;
    SpA[0] = t1;
    SpA[-1] = t1;
    SpA = SpA - 1;
    ENTER((pointer**)Node);
}

pointer plus_entry() {
    JUMP(main);
}

int main() {
    function f_main = (function)main;
    function cont = main_entry;
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}
