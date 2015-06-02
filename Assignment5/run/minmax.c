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

void minmaxone(char **file, int *index, double *max, double *min, int *n ){
 FILE *stream = fopen(*file, "r");
 char line[1024];
 int i=0;
while( fgets(line, 1024, stream))
 { 
    if(i>0){
     char *tmp = strdup(line);
     double x;
     x = getfield(tmp, *index);
     if(x< *min) *min = x;
     if(x> *max) *max = x;
     free(tmp);
    };

    i++;
 }
 *n = i;
}
