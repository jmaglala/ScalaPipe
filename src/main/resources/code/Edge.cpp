#ifndef __EDGE_CPP__
#define __EDGE_CPP__

#include "Edge.h"

uint64_t Edge::get_size()
{
    return this->m_size * sizeof(int);
}

bool Edge::is_initd()
{
    return this->m_initialized;
}

void Edge::set_buff(int * buff)
{
    if (buff == NULL)
        posix_memalign((void**)&this->m_buff,ALIGN, this->get_size());
    else
        this->m_buff = buff;
    this->m_initialized = true;
}

uint64_t Edge::get_free()
{
    return this->m_size - this->get_available();
}

#endif // __EDGE_CPP__