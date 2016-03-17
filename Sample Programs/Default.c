#include <stdio.h>
#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return((pointer) lbl)
typedef int * pointer;
typedef pointer (* function)();

pointer SpB[10000];
pointer* SpA = SpB + 9999;
int HLimit = 10000;
pointer Hp[10000];

int main () {
    function f_main = (function)main;
    function cont;
    while (cont != f_main) {
        cont = (function)(*cont)();
    }
    return 0;
}