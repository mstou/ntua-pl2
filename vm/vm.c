#include <stdio.h>

#define STACK_SIZE 1<<20
#define NEXT_INSTRUCTION goto next_instruction
#define ROWS 80
#define COLS 25

char code[ROWS][COLS]; // the array is initialized to '\0'
// we will treat '\0' as the space command, which does nothing,
// in order to skip filling the whole array with spaces

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

int main(void){
  loader();
  register int pc_x = 0, pc_y = 0; // row and collumn that the PC points to
  register int x_direction = 0, y_direction = 1; // current direction in each axis
  register int stack_top = -1;

  switch(code[pc_x][pc_y]){
next_instruction:
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '!':
    case '`':
    case '>':
      x_direction = 0;
      y_direction = 1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '<':
      x_direction = 0;
      y_direction = -1;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '^':
      x_direction = -1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case 'v':
      x_direction = 1;
      y_direction = 0;
      pc_x = ((pc_x + x_direction)%ROWS + ROWS)%ROWS;
      pc_y = ((pc_y + y_direction)%COLS + COLS)%COLS;
      NEXT_INSTRUCTION;

    case '?':
    case '_':
    case '|':
    case '"':
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
