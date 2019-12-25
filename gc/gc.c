#include <stdio.h>

#define STACK_SIZE 1<<20
#define HEAP_SIZE  1<<24
#define NEXT_INSTRUCTION goto *(labels[code[pc_x][pc_y]])
#define ROWS 25
#define COLS 80

#define pop() (top==stack-1) ? 0 : *(top--)
#define push(X) *(++top) = (X);
#define increment_pc_x() { pc_x += x_direction; if(pc_x == -1) pc_x = ROWS-1; else if(pc_x == ROWS) pc_x = 0;}
#define increment_pc_y() { pc_y += y_direction; if(pc_y == -1) pc_y = COLS-1; else if(pc_y == COLS) pc_y = 0;}
#define increment_pc() {increment_pc_x(); increment_pc_y();}

long long code[ROWS][COLS];
static long holdrand = 73L;
long long int stack[STACK_SIZE];
long long int heap[HEAP_SIZE];
// 62bit integers -- we use the 2 LSBs for flags
// integer[0] (LSB) = 1 iff this number is an address
// integer[1] (LSB) is used as a mark-bit by mark & sweep GC

void * labels[256];

int collect_garbage(long long int* top); // returns the index of the first free cons cell in the heap
void mark(long long int* top);
void mark_helper(long long int index);
int sweep();


void loader(FILE* fp){
  int i = 0, j = 0;
  char c;

  for(int i = 0; i < ROWS; i++){
    for(int j = 0; j < COLS; j++){
      code[i][j] = ' ';
    }
  }

  while((c = fgetc(fp)) != EOF){
    if(c == '\n'){
      i = (i+1)%ROWS;
      j = 0;
    }
    else{
      code[i][j] = c;
      j = (j+1)%COLS;
    }
  }
}

int random(){
  return (((holdrand = holdrand * 214013L + 2531011L) >> 16) & 0x3);
  // returns a random number between 0 and 3
}

int main(int argc, char* argv[]){

  FILE *fp = fopen(argv[1],"r");
  char c;
  long long int x;
  register int pc_x = 0;
  register int pc_y = 0; // row and collumn that the PC points to
  register int x_direction = 0;
  register int y_direction = 1; // current direction in each axis
  register long long int * top = stack-1; // top points to the top element of the stack
  register int heap_top = -1; // pointer to the current heap limit
  register long long int a,b; // local varriables to use for items that we pop out of the stack
  register int firstFree = -73;

  loader(fp);

  labels['0'] = &&label_0;
  labels['1'] = &&label_1;
  labels['2'] = &&label_2;
  labels['3'] = &&label_3;
  labels['4'] = &&label_4;
  labels['5'] = &&label_5;
  labels['6'] = &&label_6;
  labels['7'] = &&label_7;
  labels['8'] = &&label_8;
  labels['9'] = &&label_9;
  labels['+'] = &&label_add;
  labels['-'] = &&label_subtract;
  labels['*'] = &&label_multiply;
  labels['/'] = &&label_divide;
  labels['%'] = &&label_modulo;
  labels['!'] = &&label_not;
  labels['`'] = &&label_greater;
  labels['>'] = &&label_right;
  labels['<'] = &&label_left;
  labels['^'] = &&label_up;
  labels['v'] = &&label_down;
  labels['?'] = &&label_random;
  labels['_'] = &&label_horizontal_if;
  labels['|'] = &&label_vertical_if;
  labels['"'] = &&label_stringmode;
  labels[':'] = &&label_dup;
  labels['\\'] = &&label_swap;
  labels['$'] = &&label_pop;
  labels['.'] = &&label_output_int;
  labels[','] = &&label_output_char;
  labels['#'] = &&label_bridge;
  labels['g'] = &&label_get;
  labels['p'] = &&label_put;
  labels['&'] = &&label_input_int;
  labels['~'] = &&label_input_character;
  labels[' '] = &&label_space;
  labels['c'] = &&label_cons;
  labels['h'] = &&label_head;
  labels['t'] = &&label_tail;
  labels['@'] = &&label_end;

next_instruction:
  switch(code[pc_x][pc_y]){
    case '+': // add
    label_add:
      increment_pc();
      a = pop();
      b = pop();
      push( ((b>>2)+(a>>2)) << 2 );
      NEXT_INSTRUCTION;

    case '-': // subtract
    label_subtract:
      increment_pc();
      a = pop();
      b = pop();
      push(((b>>2)-(a>>2)) << 2);
      NEXT_INSTRUCTION;

    case '*': // multiply
    label_multiply:
      increment_pc();
      a = pop();
      b = pop();
      push(((b>>2)*(a>>2)) << 2);
      NEXT_INSTRUCTION;

    case '/': // divide
    label_divide:
      increment_pc();
      a = pop();
      b = pop();
      push(((b>>2)/(a>>2)) << 2);
      NEXT_INSTRUCTION;

    case '%': // modulo
    label_modulo:
      increment_pc();
      a = pop();
      b = pop();
      push(((b>>2)%(a>>2)) << 2);
      NEXT_INSTRUCTION;

    case '!': // not
    label_not:
      increment_pc();
      a = pop();
      push((((a>>2)!=0) ? 0 : 1)<<2);
      NEXT_INSTRUCTION;

    case '`': // greater
    label_greater:
      increment_pc();
      a = pop();
      b = pop();
      push((((b>>2) > (a>>2)) ? 1 : 0)<<2);
      NEXT_INSTRUCTION;

    case '>': // right
    label_right:
      x_direction = 0;
      y_direction = 1;
      increment_pc();
      NEXT_INSTRUCTION;

    case '<': // left
    label_left:
      x_direction = 0;
      y_direction = -1;
      increment_pc();
      NEXT_INSTRUCTION;

    case '^': // up
    label_up:
      x_direction = -1;
      y_direction = 0;
      increment_pc();
      NEXT_INSTRUCTION;

    case 'v': // down
    label_down:
      x_direction = 1;
      y_direction = 0;
      increment_pc();
      NEXT_INSTRUCTION;

    case '?': // random
    label_random:
      a = random();
      switch(a){
        case 0:
          goto label_left;
        case 1:
          goto label_right;
        case 2:
          goto label_down;
        case 3:
          goto label_up;
      }

    case '_': // horizontal if
    label_horizontal_if:
      a = pop();
      if((a>>2) == 0) goto label_right;
      else goto label_left;

    case '|': // vertical if
    label_vertical_if:
      a = pop();
      if((a>>2) == 0) goto label_down;
      else goto label_up;

    case '"': // stringmode
      label_stringmode:
      increment_pc();

      c = code[pc_x][pc_y];
      if( c != '"'){
        push(c<<2);
        goto label_stringmode;
      }
      increment_pc();
      NEXT_INSTRUCTION;

    case ':': // dup
    label_dup:
      increment_pc();
      a = pop();
      push(a);
      push(a);
      NEXT_INSTRUCTION;

    case '\\': // swap
    label_swap:
      increment_pc();
      a = pop();
      b = pop();
      push(a);
      push(b);
      NEXT_INSTRUCTION;

    case '$': // pop
    label_pop:
      increment_pc();
      pop();
      NEXT_INSTRUCTION;

    case '.': // output int
    label_output_int:
      increment_pc();
      a = pop();
      printf("%lld ",a>>2);
      fflush(stdout);
      NEXT_INSTRUCTION;

    case ',': // output char
    label_output_char:
      increment_pc();
      a = pop();
      printf("%c", (char) (a>>2));
      fflush(stdout);
      NEXT_INSTRUCTION;

    case '#': // bridge
    label_bridge:
      increment_pc();
      increment_pc();
      NEXT_INSTRUCTION;

    case 'g': // get
    label_get:
      increment_pc();
      a = pop();
      b = pop();
      push(code[(a>>2)][(b>>2)]<<2);
      NEXT_INSTRUCTION;

    case 'p': // put
    label_put:
      increment_pc();
      a = pop();
      b = pop();
      x = pop();
      code[(a>>2)][(b>>2)] = (x>>2);
      NEXT_INSTRUCTION;

    case '&': // input int
    label_input_int:
      increment_pc();
      scanf("%lld",&x);
      push(x<<2);
      NEXT_INSTRUCTION;

    case '~': // input_character
    label_input_character:
      increment_pc();
      scanf("%c",&c);
      push(c<<2);
      NEXT_INSTRUCTION;

    case '0':
    label_0:
      increment_pc();
      push(0<<2);
      NEXT_INSTRUCTION;

    case '1':
    label_1:
      increment_pc();
      push(1<<2);
      NEXT_INSTRUCTION;

    case '2':
    label_2:
      increment_pc();
      push(2<<2);
      NEXT_INSTRUCTION;

    case '3':
    label_3:
      increment_pc();
      push(3<<2);
      NEXT_INSTRUCTION;

    case '4':
    label_4:
      increment_pc();
      push(4<<2);
      NEXT_INSTRUCTION;

    case '5':
    label_5:
      increment_pc();
      push(5<<2);
      NEXT_INSTRUCTION;

    case '6':
    label_6:
      increment_pc();
      push(6<<2);
      NEXT_INSTRUCTION;

    case '7':
    label_7:
      increment_pc();
      push(7<<2);
      NEXT_INSTRUCTION;

    case '8':
    label_8:
      increment_pc();
      push(8<<2);
      NEXT_INSTRUCTION;

    case '9':
    label_9:
      increment_pc();
      push(9<<2);
      NEXT_INSTRUCTION;

    case ' ': // space
    label_space:
      increment_pc();
      NEXT_INSTRUCTION;

    case 'c': // cons
    label_cons:
      increment_pc();
      if(firstFree == -73){
        // this means that we haven't done any garbage collection
        // so far and we just copy cells to the heap's top
        if(heap_top > HEAP_SIZE-2){
          // we run out of space..
          // lets collect some garbage
          firstFree = -1;
          goto new_allocation;
        }
        else{
          b = pop();
          a = pop();
          heap[++heap_top] = a;
          long long int addr = (heap_top << 2) | 0x1;
          push(addr);

          heap[++heap_top] = b;
          NEXT_INSTRUCTION;
        }
      }
      else{
        // we have done garbage collection and we will use holes in the heap
        // to allocate new cells
        new_allocation:
        if(firstFree == -1){
          firstFree = collect_garbage(top);
        }
        int newCell = firstFree;
        b = pop();
        a = pop();

        firstFree = heap[newCell]>>2; // get a pointer to the next available free cell
        heap[newCell] = a;
        heap[newCell+1] = b;
        newCell = (newCell << 2) | 0x1;

        push(newCell);
        NEXT_INSTRUCTION;
      }

    case 'h': // head
    label_head:
      increment_pc();
      a = pop();
      push(heap[a>>2]);
      NEXT_INSTRUCTION;

    case 't': // tail
    label_tail:
      increment_pc();
      a = pop();
      push(heap[(a>>2)+1]);
      NEXT_INSTRUCTION;

    case '@': // end
    label_end:
      return 0;
  }

  return 0;
}

int collect_garbage(long long int* top){
  mark(top);
  return sweep();
}

void mark(long long int* top){
  long long int *p = stack;
  long long int x;

  while(p <= top){
    x = *(p++);
    // if x is a pointer to the heap explore it
    if( (x & 0x1) ){
      mark_helper(x>>2);
    }
  }
}

void mark_helper(long long int index){

  if(!(heap[index] & 0x2)){
    heap[index] = heap[index] | 0x2;
    if( heap[index] & 0x1 ) mark_helper(heap[index]>>2);
    if( heap[index+1] & 0x1 ) mark_helper(heap[index+1]>>2);
  }
}

int sweep(){
  int lastFree = -1;
  long long int x;

  for(int i=0;i<HEAP_SIZE;i+=2){
    x = heap[i];
    if(!(x & 0x2)){
      // we found some trash
      heap[i] = (lastFree<<2) | 0x1;
      lastFree = i;
    }
    else{
      heap[i] = ((heap[i]>>2)<<2) | 0x1; // remove the markbit
    }
  }

  return lastFree;
}
