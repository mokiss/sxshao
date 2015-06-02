#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>

void getfields( const char *line, int num, char **result){
 char *tok,*newline = strdup(line);
 char **bp = &newline;
 int i;
 for(i = 0; i<num; i++) {
   tok = strsep(bp, ",");
   result[i] = tok;
 }
}

void check(char **file1, char **file2, int *num1, int *num2, int *col1, int *col2, int *result){
 FILE *stream1 = fopen(*file1, "r");
 FILE *stream2 = fopen(*file2,  "r");
 char line1[1024], line2[1024];
 int i=0,j;
 while( fgets(line1, 1024, stream1) && fgets(line2, 1024, stream2))
 { 
    if(i>0){
     char *tmp1 = strdup(line1);
     char *tmp2 = strdup(line2);
     char **charp1[*num1], **charp2[*num2];

     getfields(tmp1, *num1, charp1);   
     getfields(tmp2, *num2, charp2);
     for(j=0;j<*num1 && j <*num2;j++){
       if(strcmp(charp1[ col1[j] ],charp2[ col2[j] ]) !=0 ) *result = i;
     };
     free(tmp1);free(tmp2);
    };

    i++;

 }
}