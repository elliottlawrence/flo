#include <stdio.h>

#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return((pointer) lbl)

typedef int * pointer;
typedef pointer (* function)();

pointer SpB[10000];
pointer* SpA = SpB + 9999;

int HLimit = 10000;
pointer Hp[10000];

int main();

pointer main_glob() {
  static int i = 0;
  printf("main_glob %d\n", i);

  if (i < 10) {
    i++;
    JUMP(main_glob);
  } else {
    JUMP(main);
  }
}

int main() {
  const function f_main = (function)main;
  function cont = main_glob;
  while (cont != f_main) {
    cont = (function)(*cont)();
  }
  return 0;
}
