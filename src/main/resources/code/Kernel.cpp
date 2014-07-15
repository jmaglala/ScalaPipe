#ifndef _KERNEL_CPP_
#define _KERNEL_CPP_

#include "Kernel.h"

int Kernel::get_free(int out_port)
{
    if (out_port > outputs.size()-1)
        return 0;
        
    return outputs[out_port]->get_free();
}

/*void * Kernel::allocate(int out_port)
{
    void *ptr = NULL;
    for(;;) {
        ptr = outputs[out_port]->allocate();
        if(SPLIKELY(ptr != NULL))
        {
            return ptr;
        }
    }
}*/

/*void Kernel::send(int out_port)
{
    outputs[out_port]->send();
}*/

int Kernel::get_available(int in_port)
{
    int result = 0;
    if (inputs.size() != 0)
        result = inputs[in_port]->get_available();
    return result;
}

int * Kernel::read_value(int in_port)
{
    int *ptr = NULL;
    int end_count = 0;
    for(;;)
    {
        *ptr = inputs[in_port]->read();
        if(SPLIKELY(ptr != NULL))
        {
            clock.count += 1;
            return ptr;
        }
    }
}

/*void Kernel::release(int in_port)
{
    inputs[in_port]->release();
}*/

void Kernel::set_state(uint8_t * buff)
{
    state_buff = buff;
}

void Kernel::load()
{
    volatile int curr = 0;
    for (int i=0;i<state;i++)
    {
        curr += state_buff[i];
    }
}

void Kernel::init()
{
    for(int i=0;i<state;i++)
        state_buff[i] = i;
}

#endif