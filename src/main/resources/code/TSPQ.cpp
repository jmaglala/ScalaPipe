#ifndef __TSPQ_CPP__
#define __TSPQ_CPP__

#include "TSPQ.h"


TSPQ::TSPQ(uint64_t size,size_t width)  : Edge(size+1,width)
{
    m_read_pos.store(0);
    m_write_pos.store(0);
    m_count.store(0);
    this->m_initialized = false;
    this->set_buff(NULL);
}


TSPQ::~TSPQ()
{
}

bool TSPQ::ready(uint64_t change, bool writing)
{
    auto count = m_count.load(std::memory_order_acquire);

    // We can't read/write more than is in the buffer
    if (change > this->m_size-1) 
    {
        return false;
    }
    // We're checking if the edge has enough space for the write
    if (writing)
    {
        if (count + change <= this->m_size-1)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    // otherwise we're seeing if there is enough to read
    else
    {
        if (count >= change) 
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}


bool TSPQ::empty()
{
    return m_write_pos.load(std::memory_order_acquire) == m_read_pos.load(std::memory_order_acquire);
}


bool TSPQ::full()
{
    auto curr_pos = m_write_pos.load(std::memory_order_acquire);
    return (curr_pos+1) % this->m_size == m_read_pos.load(std::memory_order_acquire);
}


void TSPQ::read(char & loc)
{
    while(empty())
        ;
    auto curr_pos = m_read_pos.load();
    loc = this->m_buff[curr_pos];
    m_read_pos.store((curr_pos +1) % this->m_size,std::memory_order_release);
    m_count--;
}


void TSPQ::write(char & val)
{
    while(full())
        ;
    auto curr_pos = m_write_pos.load();
    this->m_buff[curr_pos] = val;
    m_write_pos.store((curr_pos + 1) % this->m_size,std::memory_order_release);
    m_count++;
}

uint64_t TSPQ::get_available()
{
    return m_count.load();
}

#endif // __TSPQ_CPP__