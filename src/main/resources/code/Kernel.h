#ifndef _KERNEL_H_
#define _KERNEL_H_

class Kernel
{
private:
    Kernel() : inrate(1), outrate(1), state(0), runtime(0) 
    {}
    
public:
    int id;
    int inrate;
    int outrate;
    int state;
    int runtime;
    
    Kernel(int in, int out, int state, int rt) :
        inrate(in), outrate(out), state(state), runtime(rt)
    {}
    
    virtual void init() = 0;
    virtual void fire() = 0;
    virtual void destroy() = 0;
};

#endif // _KERNEL_H_