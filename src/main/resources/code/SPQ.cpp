#ifndef __SPQ_CPP__
#define __SPQ_CPP__

#include "SPQ.h"

SPQ::SPQ(uint64_t size, size_t width) : Edge(size+1,width)
{
    m_read_pos = 0;
    m_write_pos = 0;
    m_count = 0;
    this->m_initialized = false;
    this->set_buff(NULL);
    
}

bool SPQ::ready(uint64_t change, bool writing)
{
    if (change > this->m_size-1)
        return false;
    if (writing)
    {
        if ( m_count + change <= this->m_size -1)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        if (m_count >= change)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}

bool SPQ::empty()
{
    return m_write_pos == m_read_pos;
}

bool SPQ::full()
{
    return (m_write_pos + 1) % this->m_size == m_read_pos;
}

// A blocking read
void SPQ::read(char & loc)
{
    while(empty())
        ;
    loc = this->m_buff[m_read_pos];
    m_read_pos = (m_read_pos + 1) % this->m_size;
    m_count--;
}

// A blocking write
void SPQ::write(char & val)
{
    while(full())
        ;
    this->m_buff[m_write_pos] = val;
    m_write_pos = (m_write_pos + 1) % this->m_size;
    m_count++;
}

uint64_t SPQ::get_available()
{
    return m_count;
}

#endif //__SPQ_CPP__