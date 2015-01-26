#include <stdio.h>

double to_celsius(double fahrenheit);

main(){
  int lower = 0, upper = 120, step = 5;
  double fahr, celsius;
  for(fahr = 0; fahr <= upper; fahr += step){
    celsius = to_celsius(fahr);
    printf("celisus: %3.2f, fahrenheit: %3.2f\n", celsius, fahr);
  }
}

double to_celsius(double fahrenheit){
  return 5 * (fahrenheit - 32) / 9.0;
}
