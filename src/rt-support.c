#pragma clang diagnostic ignored "-Woverride-module"
#include <stdio.h>
#include <stdlib.h>


int getint(){
  int n;
  scanf("%d\n",&n);
  return n;
}
float getfloat(){
  float f;
  scanf("%f\n",&f);
  return f;
}

char getcharacter(){
  return getchar();
}

void printchar(char c){
  printf("%c",c);
}

void printfloat(float f){
  printf("%f\n",f);
}

void print(int n){
  printf("%d\n", n);  
}