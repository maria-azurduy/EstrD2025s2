#include <iostream>
#include "Par.h"
using namespace std;

Par consPar(int b, int c) {
    Par p;   // asigno el tipo que voy a crear, y el nombre que le voy a poner 
    p.x = b; // entro al primer valor de la tupla
    p.y = c; // entro al segundo valor de la tupla

    return p; // devuelvo lo que cree 

}



int fst(Par p) {
    return p.x;
}

/* void show(Par p) {
    cout<< "(" << p.x + ","  + p.y + ")" <<endl;
} */

int snd(Par p) {
    return p.y;
}

int maxDelPar(Par p) {
    if (p.x > p.y){
        return p.x;
    } else {
        return p.y;
    }
}

Par swap(Par p) {
    Par d;
    d.x = p.y;
    d.y = p.x;

    return d;
}

Par divisionYResto(int n, int m) {
    Par dr;
    dr.x = n / m;
    dr.y = n % m;

    return dr;
}




