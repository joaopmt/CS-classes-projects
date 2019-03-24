#include<stdio.h>
#include<math.h>

// Retorna o valor da EDO para os parametros t, y, r, k
double dydx(double t, double y, double r, double k){
	return(r*y*(1-(y/k)));
}
// Solucao analitica do PVI
double analitica(double t, double y0, double r, double k){

	return k*y0*exp(r*t) / (k + y0*(exp(r*t)-1));
}
// Metodo de Euler com valores iniciais t0 e y0, step h e intervalo [t0,limit]
void euler(double t0, double y0, double h, double limit){
    printf("\nt\t\ty_euler\t\ty_analitica\n");
    double t = t0;
    double y = y0;
    while(t <= limit){
        printf("%lf\n", t, y, analitica(t, y0, 0.5, 10));
        y += h*dydx(t, y, 0.5, 10);
        t += h;
    }
}

// Funcao principal
int main(){
    // Chamada do metodo com intervalo [t0,limit] = [0,4], y0 = 1, h = 0.05
    euler(0, 1, 0.05, 4);
    return 0;
}
