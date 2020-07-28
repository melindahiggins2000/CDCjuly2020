

libname library 'C:\MyGithub\CDCjuly2020\' ;

proc format library = library ;
   value $SEX
     'F' = 'female'  
     'I' = 'imature'  
     'M' = 'male' ;

proc datasets library = library ;
modify abalone;
   format       sex $SEX.;
quit;
