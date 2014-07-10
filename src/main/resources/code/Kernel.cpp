#ifndef _KERNEL_CPP_
#define _KERNEL_CPP_

#include "Kernel.h"

int Kernel::get_free(int out_port)
{
    if (out_port > outputs.size()-1)
        return 0;
        
    return outputs[out_port]->get_free();
}

void * Kernel::allocate(int out_port)
{
    spc_stop(&clock);
    void *ptr = NULL;
    for(;;) {
        ptr = outputs[out_port]->allocate();
        if(SPLIKELY(ptr != NULL))
        {
            spc_start(&clock);
            return ptr;
        }
        sched_yield();
    }
}

void Kernel::send(int out_port)
{
    spc_stop(&clock);
    outputs[out_port]->send();
    spc_start(&clock);
}

int Kernel::get_available(int in_port)
{
    spc_stop(&clock);
    int result = 0;
    if (inputs.size() != 0)
        result = inputs[in_port]->get_available();
    spc_start(&clock);
    return result;
    
    
}

void * Kernel::read_value(int in_port)
{
    void *ptr = NULL;
    int end_count = 0;
    spc_stop(&clock);
    for(;;)
    {
        ptr = inputs[in_port]->read_value();
        if(SPLIKELY(ptr != NULL))
        {
            clock.count += 1;
            spc_start(&clock);
            return ptr;
        }
        if(SPUNLIKELY(active_inputs == 0))
        {
            if (end_count > 1) {
                longjmp(env,1);
            }
            end_count++;
        }
        sched_yield();
    }
}

void Kernel::release(int in_port)
{
    spc_stop(&clock);
    inputs[in_port]->release();
    spc_start(&clock);
}

#endif