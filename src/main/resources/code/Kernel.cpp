#ifndef _KERNEL_CPP_
#define _KERNEL_CPP_

#include "Kernel.h"

int Kernel::get_free(int out_port)
{
    if (out_port > outputs.size()-1)
        return 0;
        
    return outputs[out_port]->get_free();
}

int Kernel::get_available(int in_port)
{
    int result = 0;
    if (inputs.size() != 0)
        result = inputs[in_port]->get_available();
    return result;
}

char * Kernel::read_value(int in_port)
{
    char *ptr = NULL;
    int end_count = 0;
    for(;;)
    {
        inputs[in_port]->read((char &) *ptr);
        if(SPLIKELY(ptr != NULL))
        {
            clock.count += 1;
            return ptr;
        }
    }
}

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

bool Kernel::fireable()
{
    bool fireable = true;
    if (outputs.size() > 0)
    {
        fireable = fireable && outputs[0]->ready(outrate,true);
    }
    if (inputs.size() > 0)
    {
        fireable = fireable && inputs[0]->ready(inrate,false);
    }
    return fireable;
}
#endif