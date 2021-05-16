int add(int* x, int* y)
{
  return *x + *y;
}
struct S{
  int a;
  int b;
};

void func(struct S b){}

int main()
{
  struct S s1;
  func(s1);
  int* a;
  int* b;
  *a = 25;
  *b=17;
  print( add(a, b) );
  return 0;
}
