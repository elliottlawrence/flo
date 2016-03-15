#define TRUE  1
#define FALSE 0
#define ENTER(c)  JUMP(**c)
#define JUMP(lbl)  return(lbl)


int main () {
    while (TRUE) {
        cont = (*cont)();
    }
}