#include <stdio.h>

#define STACK_SIZE 1<<20
#define NEXT_INSTRUCTION goto next_instruction
#define ROWS 25
#define COLS 80

char code[ROWS][COLS]; // the array is initialized to '\0'
// we will treat '\0' as the space command, which does nothing,
// in order to skip filling the whole array with spaces
static long holdrand = 73L;
signed long int stack[STACK_SIZE];

void loader(){
  int i = 0, j = 0;
  char c;
  while((c = getchar_unlocked()) != EOF){
    // printf("%c",c);
    if(c == '\n'){
      i = (i+1)%ROWS;
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
  register int pc_x = 0, pc_y = 0; // row and collumn that the PC points to
  register int x_direction = 0, y_direction = 1; // current direction in each axis
  register int top = -1; // top points to the top element of the stack
  register unsigned long int a,b; // local varriables to use for items that we pop out of the stack
  char c;
  int x;

  // TODO : implement direct threading & make PC a pointer
  // -- change pop implementation to help branch prediction on (top==-1)
  // reminder: GNU C allows global register values
next_instruction:
  switch(code[pc_x][pc_y]){
    case '+': // add
    label_add:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = b+a;
      NEXT_INSTRUCTION;

    case '-': // substract
    label_substract:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = b-a;
      NEXT_INSTRUCTION;

    case '*': // multiply
    label_multiply:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = b*a;
      NEXT_INSTRUCTION;

    case '/': // divide
    label_divide:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = b/a;
      NEXT_INSTRUCTION;

    case '%': // modulo
    label_modulo:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = b%a;
      NEXT_INSTRUCTION;

    case '!': // not
    label_not:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      stack[++top] = (a!=0) ? 0 : 1;
      NEXT_INSTRUCTION;

    case '`': // greater
    label_greater:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[top++] = (b > a) ? 1 : 0;
      NEXT_INSTRUCTION;

    case '>': // right
    label_right:
      x_direction = 0;
      y_direction = 1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '<': // left
    label_left:
      x_direction = 0;
      y_direction = -1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '^': // up
    label_up:
      x_direction = -1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case 'v': // down
    label_down:
      x_direction = 1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
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
      a = (top==-1) ? 0 : stack[top--];
      if(a == 0) goto label_right;
      else goto label_left;

    case '|': // vertical if
    label_vertical_if:
      a = (top==-1) ? 0 : stack[top--];
      if(a == 0) goto label_down;
      else goto label_up;

    case '"': // stringmode
      label_stringmode:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;

      c = code[pc_x][pc_y];
      if( c != '"'){
        stack[++top] = c;
        goto label_stringmode;
      }
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case ':': // dup
    label_dup:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top];
      stack[++top] = a;
      NEXT_INSTRUCTION;

    case '\\': // swap
    label_swap:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = a;
      stack[++top] = b;
      NEXT_INSTRUCTION;

    case '$': // pop
    label_pop:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      if(top != -1) top--;
      NEXT_INSTRUCTION;

    case '.': // output int
    label_output_int:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      printf("%ld ",a);
      NEXT_INSTRUCTION;

    case ',': // output char
    label_output_char:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      printf("%c", (char) a);
      NEXT_INSTRUCTION;

    case '#': // bridge
    label_bridge:
      pc_x = ((pc_x + 2*x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + 2*y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case 'g': // get
    label_get:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      stack[++top] = code[a][b];
      NEXT_INSTRUCTION;

    case 'p': // put
    label_put:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      a = (top==-1) ? 0 : stack[top--];
      b = (top==-1) ? 0 : stack[top--];
      x = (top==-1) ? 0 : stack[top--];
      code[a][b] = (char) x;
      NEXT_INSTRUCTION;

    case '&': // input int
    label_input_int:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      scanf("%d",&x);
      stack[top++] = x;
      NEXT_INSTRUCTION;

    case '~': // input_character
    label_input_character:
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      scanf("%c",&c);
      stack[top++] = c;
      NEXT_INSTRUCTION;

    case '0':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 0;
      NEXT_INSTRUCTION;

    case '1':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 1;
      NEXT_INSTRUCTION;

    case '2':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 2;
      NEXT_INSTRUCTION;

    case '3':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 3;
      NEXT_INSTRUCTION;

    case '4':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 0;
      NEXT_INSTRUCTION;

    case '5':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 5;
      NEXT_INSTRUCTION;

    case '6':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 6;
      NEXT_INSTRUCTION;

    case '7':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 7;
      NEXT_INSTRUCTION;

    case '8':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 8;
      NEXT_INSTRUCTION;

    case '9':
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      stack[++top] = 9;
      NEXT_INSTRUCTION;

    case '@': // end
    label_end:
      return 0;
  }


  return 0;
}
