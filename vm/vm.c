#include <stdio.h>

#define STACK_SIZE 1<<20
#define NEXT_INSTRUCTION goto next_instruction
#define ROWS 80
#define COLS 25

char code[ROWS][COLS]; // the array is initialized to '\0'
// we will treat '\0' as the space command, which does nothing,
// in order to skip filling the whole array with spaces
static long holdrand = 73L;
signed long int stack[STACK_SIZE];

void loader(){
  int i = 0, j = 0;
  char c;
  while((c = getchar_unlocked()) != EOF){
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

  // TODO : implement direct threading & make PC a pointer
  // reminder: GNU C allows global register values
next_instruction:
  switch(code[pc_x][pc_y]){
    case '+':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      stack[++top] = a+b;
      NEXT_INSTRUCTION;

    case '-':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      stack[++top] = a-b;
      NEXT_INSTRUCTION;

    case '*':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      stack[++top] = a*b;
      NEXT_INSTRUCTION;

    case '/':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      stack[++top] = a/b;
      NEXT_INSTRUCTION;

    case '%':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      stack[++top] = a%b;
      NEXT_INSTRUCTION;

    case '!':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( a != 0 ) stack[++top] = 0;
      else stack[++top] = 1;
      NEXT_INSTRUCTION;

    case '`':
      if( top == -1 ) a = 0;
      else a = stack[top--];
      if( top == -1 ) b = 0;
      else b = stack[top--];

      a = (a > b) ? 1 : 0;
      stack[++top] = a;
      NEXT_INSTRUCTION;

    case '>':
    label_right:
      x_direction = 0;
      y_direction = 1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '<':
    label_left:
      x_direction = 0;
      y_direction = -1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '^':
    label_up:
      x_direction = -1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case 'v':
    label_down:
      x_direction = 1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '?':
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

    case '_':
      if( top == -1 ) a = 0;
      else a = stack[top--];

      if(a == 0) goto label_right;
      else goto label_left;

    case '|':
      if( top == -1 ) a = 0;
      else a = stack[top--];

      if(a == 0) goto label_down;
      else goto label_up;

    case '"':
    label_stringmode_on:
    label_stringmode_off:

    case ':':
    case '\\':
    case '$':
    case '.':
    case ',':
    case '#':
    case 'g':
    case 'p':
    case '&':
    case '~':
    case '@':
      return 0;
  }



  return 0;
}
