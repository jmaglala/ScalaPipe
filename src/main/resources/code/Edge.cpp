#ifndef __EDGE_CPP__
#define __EDGE_CPP__

#include "Edge.h"

uint64_t Edge::get_size()
{
    return m_size * width;
}

bool Edge::is_initd()
{
    return m_initialized;
}

void Edge::set_buff(char * buff)
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

uint64_t Edge::size()
{
    return this->m_size;
}
#endif // __EDGE_CPP__