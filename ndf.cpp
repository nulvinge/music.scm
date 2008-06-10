#include <iostream>
#include <cmath>
using namespace std;

const int nbuckets = 100;
const double range = 5;
const int ndatapoints = 1000000;

struct double_pair{
    double x;
    double y;
};
/*
double box_muller(){
    double a = (double(rand())/RAND_MAX);
    double b = (double(rand())/RAND_MAX);
    return sqrt(-2*log(a))*sin(2*MATH_PI*b)
}*/

double rnd(){
    return (double(rand())/RAND_MAX);
}
double unirnd(){
    return rnd()*2-1;
}

double_pair marsaglia(){
    double s = 0,
           x = 0,
           y = 0;
    do{
        x = unirnd();
        y = unirnd();
        s = x*x+y*y;
    }while(s>=1);
    double_pair a;
    s = sqrt(-2*log(s)/s);
    a.x = x*s;
    a.y = y*s;
    return a;
}

double r(){
    double x = rnd();
    return -pow(x, 0.5); //straigth line
}

double_pair random_pair(){
    //return marsaglia();
    double_pair a;
    a.x = r();
    a.y = r();
    return a;
}


double f(){
    double x = rnd();
//    return exp(x);
    return x*x;
//    return (x*2-1)*x;
//    return exp((t*t)/2);
//    return exp(u*t+s*s*t*t/2);
}

int* buckets = new int[nbuckets];
void add_to_bucket(double x){
    x+=range;
    int j = 0;
    while(x>0){
        x -= 2*range/((double)nbuckets);
        j++;
    }
    buckets[j]++;
}
int main(){
    for(int i = 0; i<ndatapoints/2; ++i){
        double_pair a = random_pair();
        add_to_bucket(a.x);
        add_to_bucket(a.y);
    }

    for(int i = 0; i<nbuckets;++i)
        cout << i*2*range/((double)nbuckets) -range << '\t'
             << (buckets[i]/((double)(ndatapoints*range*2/nbuckets))) << '\n';
}
