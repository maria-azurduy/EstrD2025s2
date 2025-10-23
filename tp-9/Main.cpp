#include <iostream>
using namespace std;
#include "Par.h"

int main() {

  Par p = consPar(2,4);
  cout << "(" << fst(p) << "," << snd(p) << ")" << endl;
  Par p1 = divisionYResto(4,2);
 /*  cout << fst(p) << endl;
  cout << snd(p) << endl; */
  /* cout << p.y << endl; */
    
/*   int x = 17;
  char c = 'a';
  cout << x << endl;
  cout << c << endl; */
}

