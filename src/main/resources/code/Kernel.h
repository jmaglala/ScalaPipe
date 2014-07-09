#ifndef _KERNEL_H_
#define _KERNEL_H_

#include "Edge.h"
#include "ScalaPipe.h"
#include <vector>
#include <sched.h>
class Kernel
{
    SPC clock;
    
    int id;
    int inrate;
    int outrate;
    int state;
    int runtime;
 
    int active_inputs;
    
    std::vector<Edge> inputs;
    std::vector<Edge> outputs;
    
    Kernel(int in, int out, int state, int rt) :
        inrate(in), outrate(out), state(state), runtime(rt)
    {}
    
    virtual void init() = 0;
    virtual void fire() = 0;
    virtual void destroy() = 0;
    
    int get_free(int out_port);
    void * allocate(int out_port);
    void send(int out_port);
    int get_available(int in_port);
    void * read_vaule(int in_port);
    void release(int in_port);
};

#endif // _KERNEL_H_