#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_ITERATION 22 // limite de iteracoes para o metodo

// calcula Butler-Volmer em x com a = 0.2 e b = 2
double f(double x){
    double a = 0.2;
	double b = 2;

  	return (exp(a*x) - exp((a-1)*x) - b);
}

// metodo da bisseccao no intervalo [a, b] e precisao e na funcao f
double bisection(double a, double b, double e){
	int k = 1;
    double x = a;
	while((b-a) >= e && k++ <= MAX_ITERATION){
      	x = (a+b)/2;
		if(f(a)*f(x) < 0)
        	b = x;
		else
			a = x;
    }
    return x;
}

// testador
int main (){
	double a, b, x;
    scanf("%lf %lf", &a, &b);
	x = bisection(a, b, 0.1);
    printf("%.8lf\n", x);
    return 0;
}
