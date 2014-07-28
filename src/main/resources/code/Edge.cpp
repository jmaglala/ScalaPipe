#ifndef __EDGE_CPP__
#define __EDGE_CPP__

#include "Edge.h"

template<typename T>
uint64_t Edge<T>::get_size()
{
    return this->m_size * sizeof(T);
}

bool Edge::is_initd()
{
    return this->m_initialized;
}

template<typename T>
void Edge<T>::set_buff(T * buff)
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