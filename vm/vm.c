#include <stdio.h>

#define STACK_SIZE 1<<20
#define NEXT_INSTRUCTION goto *(labels[code[pc_x][pc_y]])
#define ROWS 25
#define COLS 80

#define pop() (top==stack-1) ? 0 : *(top--)
#define push(X) *(++top) = (X)
#define increment_pc_x() {  pc_x += x_direction; if(pc_x == -1) pc_x = ROWS-1; else if(pc_x == ROWS) pc_x = 0;}
#define increment_pc_y() {  pc_y += y_direction; if(pc_y == -1) pc_y = COLS-1; else if(pc_y == COLS) pc_y = 0;}
#define increment_pc() {increment_pc_x(); increment_pc_y();}

long long code[ROWS][COLS];
static long holdrand = 73L;
signed long int stack[STACK_SIZE];
void * labels[256];

// int pc_x = 0;
// int pc_y = 0; // row and collumn that the PC points to
// int x_direction = 0;
// int y_direction = 1; // current direction in each axis
//
// static inline void increment_pc() {
//   pc_x += x_direction;
//   pc_y += y_direction;
//
//   if(pc_x < 0) pc_x += ROWS;
//   else if(pc_x >= ROWS) pc_x -= ROWS;
//
//   if(pc_y < 0) pc_y += COLS;
//   else if(pc_y >= COLS) pc_y -= COLS;
//
// }

void loader(){
  int i = 0, j = 0;
  char c;

  for(int i = 0; i < ROWS; i++){
    for(int j = 0; j < COLS; j++){
      code[i][j] = ' ';
    }
  }

  while((c = getchar_unlocked()) != EOF){
    // printf("%c",c);
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

int main(void){
  loader();
  char c;
  signed long int x;
  register int pc_x = 0;
  register int pc_y = 0; // row and collumn that the PC points to
  register int x_direction = 0;
  register int y_direction = 1; // current direction in each axis
  register signed long int * top = stack-1; // top points to the top element of the stack
  register signed long int a,b; // local varriables to use for items that we pop out of the stack

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
  labels['@'] = &&label_end;

  // TODO : implement direct threading & make PC a pointer
  // -- change pop implementation to help branch prediction on (top==-1)
  // reminder: GNU C allows global register values
next_instruction:
  switch(code[pc_x][pc_y]){
    case '+': // add
    label_add:
      increment_pc();
      a = pop();
      b = pop();
      push(b+a);
      NEXT_INSTRUCTION;

    case '-': // subtract
    label_subtract:
      increment_pc();
      a = pop();
      b = pop();
      push(b-a);
      NEXT_INSTRUCTION;

    case '*': // multiply
    label_multiply:
      increment_pc();
      a = pop();
      b = pop();
      push(b*a);
      NEXT_INSTRUCTION;

    case '/': // divide
    label_divide:
      increment_pc();
      a = pop();
      b = pop();
      push(b/a);
      NEXT_INSTRUCTION;

    case '%': // modulo
    label_modulo:
      increment_pc();
      a = pop();
      b = pop();
      push(b%a);
      NEXT_INSTRUCTION;

    case '!': // not
    label_not:
      increment_pc();
      a = pop();
      push((a!=0) ? 0 : 1);
      NEXT_INSTRUCTION;

    case '`': // greater
    label_greater:
      increment_pc();
      a = pop();
      b = pop();
      push((b > a) ? 1 : 0);
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
      if(a == 0) goto label_right;
      else goto label_left;

    case '|': // vertical if
    label_vertical_if:
      a = pop();
      if(a == 0) goto label_down;
      else goto label_up;

    case '"': // stringmode
      label_stringmode:
      increment_pc();

      c = code[pc_x][pc_y];
      // if( c == '\0' ) c = ' ';
      // if(c >= '0' && c <= '9'){
      //   stack[++top] = c - '0';
      //   goto label_stringmode;
      // }
      if( c != '"'){
        push(c);
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
      printf("%ld ",a);
      NEXT_INSTRUCTION;

    case ',': // output char
    label_output_char:
      increment_pc();
      a = pop();
      printf("%c", (char) a);
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
      // if(code[a][b] == '\0') code[a][b] = ' ';
      push(code[a][b]);
      NEXT_INSTRUCTION;

    case 'p': // put
    label_put:
      increment_pc();
      a = pop();
      b = pop();
      x = pop();
      code[a][b] = x;
      NEXT_INSTRUCTION;

    case '&': // input int
    label_input_int:
      increment_pc();
      scanf("%ld",&x);
      push(x);
      NEXT_INSTRUCTION;

    case '~': // input_character
    label_input_character:
      increment_pc();
      scanf("%c",&c);
      push(c);
      NEXT_INSTRUCTION;

    case '0':
    label_0:
      increment_pc();
      push(0);
      NEXT_INSTRUCTION;

    case '1':
    label_1:
      increment_pc();
      push(1);
      NEXT_INSTRUCTION;

    case '2':
    label_2:
      increment_pc();
      push(2);
      NEXT_INSTRUCTION;

    case '3':
    label_3:
      increment_pc();
      push(3);
      NEXT_INSTRUCTION;

    case '4':
    label_4:
      increment_pc();
      push(4);
      NEXT_INSTRUCTION;

    case '5':
    label_5:
      increment_pc();
      push(5);
      NEXT_INSTRUCTION;

    case '6':
    label_6:
      increment_pc();
      push(6);
      NEXT_INSTRUCTION;

    case '7':
    label_7:
      increment_pc();
      push(7);
      NEXT_INSTRUCTION;

    case '8':
    label_8:
      increment_pc();
      push(8);
      NEXT_INSTRUCTION;

    case '9':
    label_9:
      increment_pc();
      push(9);
      NEXT_INSTRUCTION;

    case ' ': // space
    label_space:
      increment_pc();
      NEXT_INSTRUCTION;

    case '@': // end
    label_end:
      return 0;
  }

  return 0;
}
