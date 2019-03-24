#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_ITERATION 20 // limite de iteracoes para o metodo

// calcula Butler-Volmer em x com a = 0.2 e b = 2
double f(double x){
	double b = 2;
  	double a = 0.2;

  	return (exp(a*x) - exp((a-1)*x) - b);
}

// calcula derivada de Butler-Volmer em x com a = 0.2
double df(double x){
  	double a = 0.2;

  	return (exp((a-1)*x)*(a*(exp(x)-1)+1));
}

// metodo de newton-raphson no ponto inicial x e precisoes e1 e e2
double newton(double x, double e1, double e2){
	int k = 1;
    double x0, x1;

    x0 = x;

    if(fabs(f(x0)) < e1)
    	return x0;

    while(k <= MAX_ITERATION){
        x1 = x0 - f(x0)/df(x0);
        if(fabs(f(x0)) < e1 || fabs(x1-x0) < e2)
            return x1;
        x0 = x1;
        k++;
    }
    return x0;
}

// testador
int main (){
	double x0, x;
    scanf("%lf", &x0);
	x = newton(x0, 0, 0);
    printf("%.8lf\n", x);
    return 0;
}
