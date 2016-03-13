#include <stdbool.h>
#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return(lbl)


int main () {
    while (true) {
        cont = (*cont)();
    }
}