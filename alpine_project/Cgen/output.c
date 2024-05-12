#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include  <stdbool.h>
#include "builtin.h"


int _fXI(){
   return 42;

}
int main(){
  PRINT(_fXI());

  return 0;
}