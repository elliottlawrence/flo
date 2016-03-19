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

pointer main_entry() {
  static int i = 0;
  printf("main_glob %d\n", i);

  if (i < 10) {
    i++;
    JUMP(main_entry);
  } else {
    JUMP(main);
  }
}

int main() {
  const function f_main = (function)main;
  function cont = main_entry;
  while (cont != f_main) {
    cont = (function)(*cont)();
  }
  return 0;
}
