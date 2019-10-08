
int x = 10;


struct a {
  int x;
  int y;
} aa;

struct {
  int z;
} b;

typedef struct {
  int x;
} c;
c cc;

void fxn()
{
  aa.x = aa.y + b.z + cc.x;
}

static struct {
  int a;
};

static struct {
} a;




