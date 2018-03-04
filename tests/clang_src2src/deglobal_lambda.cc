
int x = 0;

struct A {
  int x;
} aInst{};

void fxn()
{
  auto l1 = [&]{
    aInst.x = 0;
    x += 5;
  };

  auto l2 = [=]{
    aInst.x = 0;
    x += 5;
  };
  
  auto l3 = []{
    aInst.x = 0;
    x += 5;
  };

  auto l4 = [aInst=aInst]{
    int y = aInst.x;
    x += 5;
  };

  auto l5 = [x=x]{
    aInst.x = 0;
    int y = x;
  };

  auto l6 = [=,x=x]{
    aInst.x = 0;
    int y = x;
  };
}


