
int init(int a);
int c = 0;
static int x = init(c);

int initVoid();
static int y = initVoid();


