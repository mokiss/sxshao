#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>

double getfield( const char *line, int num){
 char *tok, *newline = strdup(line);
 char **bp = &newline;
 int i;
 for(i = 0; i<num; i++) tok = strsep(bp, ",");
 double target = strtod(tok, NULL);
 return(target);
}
double getdif( const char *line, int num1, int num2){
 char *tok, *newline = strdup(line);
 char **bp = &newline;
 double dif;
 int i;
 for(i = 0; i<num2; i++) {
  tok = strsep(bp, ",");
  if( i == (num1-1) ) dif = -strtod(tok, NULL);
  if( i == (num2-1) ) dif = dif + strtod(tok, NULL);
 }
 return(dif);
}

void reg(char **file1, char **file2,double *sum, double *inter, double *square, int *n ){
 FILE *stream1 = fopen(*file1, "r");
 FILE *stream2 = fopen(*file2,  "r");
 char line1[1024], line2[1024];
 int i=0;
while( fgets(line1, 1024, stream1) && fgets(line2, 1024, stream2))
 { 
    if(i>0){
     char *tmp1 = strdup(line1), *tmp2 = strdup(line2);
     double x,y;
     y = getdif(tmp2, 10,11);
     x = getfield(tmp1, 9);
     sum[0] = sum[0] + x;
     sum[1] = sum[1] + y;
     *inter = *inter + x*y;
     *square = *square + x*x;
     free(tmp1);free(tmp2);
    };

    i++;

 }
 *n = i;
}