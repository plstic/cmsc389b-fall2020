#include <stdio.h>
#include <stdlib.h>

int main(int argv, char** argc){
  char c;
  int pos;
  int count = 0;
  char  pass[9];
  char  passin[9];
  strcpy(pass,"password");
  while(count < 3){
    pos = 0;
    printf("password: ");
    while((c=getchar()) != '\n' && c != EOF){
      passin[pos++] = c; 
    }
    passin[8] = '\0';
    if(strcmp(pass,passin) == 0){
      printf("success!\n");    
      return 0;
    }
    count++;
  }
  printf("failed\n");
  return 1;
}
