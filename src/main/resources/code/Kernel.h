class Kernel
{
private:
    Kernel() : inrate(1), outrate(1), state(0), runtime(0) 
    {}
    
public:
    int inrate;
    int outrate;
    int state;
    int runtime;
    
    Kernel(int in, int out, int state, int rt) :
        inrate(in), outrate(out), state(state), runtime(rt)
    {}
};