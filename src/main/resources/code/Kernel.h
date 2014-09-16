#ifndef _KERNEL_H_
#define _KERNEL_H_

#include "Edge.h"
#include "ScalaPipe.h"
#include <vector>
#include <sched.h>


static int kid = 0;
class Kernel
{
public:
    SPC clock;
    jmp_buf env;
    volatile uint32_t active_inputs;
    
    int id;
    int inrate;
    int outrate;
    int state;
    int runtime;
 
    std::vector<Edge*> inputs;
    std::vector<Edge*> outputs;
    
    Kernel(int _in, int _out, int _state, int _rt) :
        id(kid++),inrate(_in), outrate(_out), state(_state), runtime(_rt)
    {}
    ~Kernel()
    {}
    
    virtual void run() = 0;
    
    virtual int get_free(int out_port) = 0;
    virtual int get_available(int in_port) = 0;
    virtual char * read_value(int in_port) = 0;
    virtual bool fireable() = 0;
};

#endif // _KERNEL_H_