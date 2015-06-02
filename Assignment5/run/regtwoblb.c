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

void regtwosamp(char **file1, char **file2, int *yindex, int *xindex ,double *sum, 
  double *square, double *interx, double *interxy, 
 int *samp, int *weight, int *stop, int *n){
 FILE *stream1 = fopen(*file1, "r");
 FILE *stream2 = fopen(*file2,  "r");
 char line1[1024], line2[1024];
 int i=0,j=0;
while( fgets(line1, 1024, stream1) && fgets(line2, 1024, stream2))
 { 
    if(i>0 && i == samp[j] ){
     char *tmp1 = strdup(line1), *tmp2 = strdup(line2);
     double x1,x2,y;
     y = getdif(tmp2, yindex[0],yindex[1]);
     x1 = getfield(tmp1, xindex[0]);
     x2 = getfield(tmp2, xindex[1]);
     sum[0] = sum[0] + x1*weight[j];
     sum[1] = sum[1] + x2*weight[j];
     sum[2] = sum[2] + y*weight[j];
     square[0] = square[0] + x1*x1*weight[j];
     square[1] = square[1] + x2*x2*weight[j];
     interx[0] = interx[0] + x1*x2*weight[j];
     interxy[0] = interxy[0] + x1*y*weight[j];
     interxy[1] = interxy[1] + x2*y*weight[j];
     free(tmp1);free(tmp2);
     j++;
    };

    i++;
 }
 *stop = j;
 *n = i;
}