int add(int* x, int* y)
{
  return *x + *y;
}

int main()
{
  int* a;
  int* b;
  *a = 25;
  *b=17;
  print( add(a, b) );
  return 0;
}
