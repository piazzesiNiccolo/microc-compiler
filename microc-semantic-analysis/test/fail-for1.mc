int main()
{
  int i;
  for ( ; true ; ) {} /* OK: Forever */

 

  for (j = 0; i < 10 ; i = i + 1) {} /* j undefined */

  
}
