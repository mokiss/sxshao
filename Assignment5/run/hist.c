#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>

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

void minmaxdif(char **file, int *index, double *max, double *min, int *n){
 FILE *stream = fopen(*file, "r");
 char line[1024];
 int i=0;
 while( fgets(line, 1024, stream))
 { 
    if(i>0){
     char *tmp = strdup(line);
     double x;
     x = getdif(tmp, index[0], index[1]);
     if(x< *min) *min = x;
     if(x> *max) *max = x;
     free(tmp);
    };

    i++;
 }
 *n = i;
}

void gethist(char **file,int *target, int *result, double *bound, double *width){
 FILE *stream = fopen(*file, "r");
 char line[1024];
 int i=0, j;
 double d;
 while( fgets(line, 1024, stream))
 { 
    if(i>0){
     char *tmp = strdup(line);
     d = getdif(tmp, target[0], target[1]);
     if(d> bound[0]  && d< bound[1]){
     j = floor( (d-bound[0])/ (*width));
     result[j] = result[j] + 1;
     };
    free(tmp);
    };

    i++;

 }
}