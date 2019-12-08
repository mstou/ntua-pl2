#include <stdio.h>

char code[80][25]; // the array is initialized to '\0'
// we will treat '\0' as the space command, which does nothing,
// in order to skip filling the whole array with spaces

void loader(){
  int i = 0, j = 0;
  char c;
  while((c = getchar_unlocked()) != EOF){
    if(c == '\n'){
      i = (i+1)%25;
    }
    else{
      code[i][j] = c;
      j = (j+1)%80;
    }
  }
}

int main(void){
  loader();
  return 0;
}
